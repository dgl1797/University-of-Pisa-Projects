import { Request, Response } from "express";
import { ObjectId } from "mongodb";
import { mongodao, neo4jdao, redisdao } from "../../daos";
import { IPlaylist, IUser, Playlist, Session, User } from "../../models";
import { tokenize, transformError, validateSchema } from "../../utils";
import {
  userPopularityScoreQuery,
  bestContributorToUserPopularityScoreQuery,
  userPopularityScoreByOwnPlaylist,
} from "./queries";
import * as validators from "./validators";

export const getUser = async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    const replyData: any = {};
    const userData: IUser = await User.findOne({ _id: new ObjectId(userId) });
    if (!userData) throw { code: 404, message: "User not found" };
    replyData.user = {
      _id: userId,
      username: userData.username,
      isValidator: userData.isValidator,
    };
    const foundUser = new User(userData);
    const neo4jresult = await neo4jdao("READ").run(
      `MATCH (u:User{_id: "${userId}"})-[l:LIKES]->(n) return count(l)`
    );
    replyData.likesCount = neo4jresult.records?.[0]?.["_fields"]?.[0]?.low ?? 0;
    const { db, instance } = await mongodao();
    replyData.numberOfPrivate = await db
      .collection("playlists")
      .find({ author: foundUser.username, isPublic: false })
      .count();
    replyData.numberOfPublic = await db
      .collection("playlists")
      .find({ author: foundUser.username, isPublic: true })
      .count();
    replyData.popularityScore =
      (await neo4jdao("READ").run(userPopularityScoreQuery(userId))).records?.[0]?.[
        "_fields"
      ]?.[0] ?? 0;
    const bestContributorResult = await neo4jdao("READ").run(
      bestContributorToUserPopularityScoreQuery(userId)
    );
    const neoData = {
      neoId:
        bestContributorResult?.records?.[0]?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? undefined,
      neoContrib: bestContributorResult?.records?.[0]?.["_fields"]?.[1] ?? 0,
    };
    const bcMongo: IUser = await User.findOne({ _id: new ObjectId(neoData.neoId) });
    replyData.bestContributor = {
      username: bcMongo?.username ?? "",
      contribution: neoData.neoContrib,
    };
    await instance.close();
    return res.status(200).json({ replyData });
  } catch (error) {
    const { code, message } = transformError(error);
    res.status(code).json({ message });
  }
};

export const updateUser = async (req: Request, res: Response) => {
  try {
    const { userId } = req.params;
    validateSchema(req.body, validators.updateSchema);
    const newUsername = req.body.username;
    const userData: IUser = await User.findOne({ _id: new ObjectId(userId) });
    if (!userData) throw { code: 404, message: "User not found" };
    const oldUsername = userData.username;
    const updatedUser = new User({ ...userData, username: newUsername });
    await updatedUser.update();
    const newToken = tokenize({
      username: updatedUser.username,
      _id: updatedUser._id,
      isValidator: updatedUser.isValidator,
    });
    await Playlist.updateMany({ author: oldUsername }, { $set: { author: `${newUsername}` } });
    const sessionToken = new Session(newToken);
    await sessionToken.setExpire(userData._id.toString(), 1000 * 3600 * 6);
    return res.status(200).json({
      updatedUser: {
        username: updatedUser.username,
        _id: updatedUser._id.toString(),
        isValidator: updatedUser.isValidator,
      },
      newToken,
    });
  } catch (error) {
    const { code, message } = transformError(error);
    res.status(code).json({ message });
  }
};

export const deleteUser = async (req: Request, res: Response) => {
  try {
    const toDelete: IUser = await User.findOne({ _id: new ObjectId(`${req.params.userId}`) });
    if (!toDelete) throw { code: 404, message: "User not found" };
    const privatePlaylists = await Playlist.find({
      author: `${toDelete.username}`,
      isPublic: false,
    });
    let ppToDelete: any = new Array(0);
    let ppToDeleteNeo: any = new Array(0);
    privatePlaylists.forEach((element) => {
      ppToDelete.push(element._id);
      ppToDeleteNeo.push(`p._id = "${element._id.toString()}"`);
    });
    ppToDeleteNeo = ppToDeleteNeo.join(" OR ");
    if (ppToDelete.length > 0) {
      const resDelPlay = await neo4jdao("WRITE").run(
        `match (p:Playlist) where ${ppToDeleteNeo} detach delete p`
      );
    } //delete playlists neo
    const resDelAcc = await neo4jdao("WRITE").run(
      `match (u:User) where u._id="${toDelete._id.toString()}" detach delete u`
    ); //delete user neo
    const updateToNull = await Playlist.updateMany(
      { author: `${toDelete.username}`, isPublic: true },
      { $set: { author: null } }
    ); //change author of public playlis to null
    const deletedPlaylists = await Playlist.deleteMany({ _id: { $in: ppToDelete } }); //delete private playlist mongo
    const deletedUser = await User.deleteOne({ _id: new ObjectId(`${req.params.userId}`) }); //delete user mongo
    const sessionToDelete = new Session();
    await sessionToDelete.delete(toDelete._id.toString());
    res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = transformError(error);
    res.status(code).json({ message });
  }
};

export const getUsersRank = async (req: Request, res: Response) => {
  try {
    const popularUser = await neo4jdao("READ").run(userPopularityScoreByOwnPlaylist);
    const IDU: { _id: string; score: number }[] = [];
    for (let entries of popularUser?.records ?? []) {
      IDU.push({
        _id: entries?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? "",
        score: entries?.["_fields"]?.[1] ?? 0,
      });
    }
    const usersArray: IUser[] = await User.find({
      _id: { $in: IDU.map((idp) => new ObjectId(idp._id)) },
    });
    type replyData = { _id: string; username: string; score: number };
    const reply: replyData[] = usersArray.map((u) => ({
      _id: u._id.toString(),
      username: u.username,
      score: IDU.find((idp) => idp._id === u._id.toString())?.score ?? 0,
    }));
    return res.json({ reply: reply.sort((el1, el2) => el2.score - el1.score) });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};
