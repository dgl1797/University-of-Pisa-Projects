import { Request, Response } from "express";
import { ObjectId } from "mongodb";
import { neo4jdao } from "../../daos";
import {
  EditedSong,
  IEditedSong,
  IPlaylist,
  ISong,
  IUser,
  Playlist,
  ReducedSong,
  Song,
  User,
} from "../../models";
import { transformError, untokenize, validateSchema } from "../../utils";
import {
  hopsSuggestion,
  likeSuggestion,
  playlistPopularityScore,
  playlistsRanks,
  sortedPaginationByLikesCount,
} from "./queries";
import {
  isPublicSchema,
  patchPlaylistSchema,
  QueryModel,
  querySchema,
  SortBy,
  sortSchema,
} from "./validators";
import errorCodes from "../../configs/errorCodes.json";

export const getPlaylistsByRanking = async (req: Request, res: Response) => {
  try {
    const scoreList = await neo4jdao("READ").run(playlistsRanks);
    const IDP: { _id: string; score: number }[] = [];
    for (let entries of scoreList?.records ?? []) {
      IDP.push({
        _id: entries?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? "",
        score: entries?.["_fields"]?.[1] ?? 0,
      });
    }
    const playlistsArray: IPlaylist[] = await Playlist.find({
      _id: { $in: IDP.map((idp) => new ObjectId(idp._id)) },
    });
    type replyData = { _id: string; title: string; score: number; author: string };
    const reply: replyData[] = playlistsArray.map((p) => ({
      _id: p._id.toString(),
      author: p.author,
      title: p.title,
      score: IDP.find((idp) => idp._id === p._id.toString())?.score ?? 0,
    }));
    return res.json({ reply: reply.sort((el1, el2) => el2.score - el1.score) });
  } catch (err) {
    const { code, message } = transformError(err);
    return res.status(code).json({ message });
  }
};

export const getPlaylists = async (req: Request, res: Response) => {
  try {
    const pageSize = 20;
    if (!req.query?.page) throw { code: 400, message: "invalid page" };
    const publicQuery = req?.query?.publicQuery ?? undefined;
    const parsedPublic: { isPublic: boolean; username: string; isMine: boolean } = JSON.parse(
      (publicQuery as string) ?? "{}"
    );
    const user: IUser = await User.findOne({ username: parsedPublic.username });
    if (!user) throw { code: 404, message: `${parsedPublic.username} does not exist` };
    validateSchema(parsedPublic, isPublicSchema);
    const pageNumber = parseInt(req?.query?.page as string);
    if (typeof pageNumber !== "number") throw { code: 400, message: "invalid page" };
    const queryString = req?.query?.q ?? JSON.stringify({ isFilter: true });
    const queryParams: QueryModel = JSON.parse(queryString as any);
    validateSchema(queryParams, querySchema);
    const sortString = req?.query?.s ?? JSON.stringify({});
    const sortParams: SortBy = JSON.parse(sortString as any);
    validateSchema(sortParams, sortSchema);
    const aggregation: any[] = [];
    // FILTERS HANDLING
    if (queryParams.isFilter) {
      let PIDs: any = undefined;
      let shouldTakeLikes = true;
      let shouldNotPaginate = false;
      if (sortParams.likes || queryParams.liked) {
        // if is asking for something related to likes neo4j executes first
        shouldTakeLikes = false;
        // shouldNotPaginate tells neo4j to not execute the pagination because other filters
        // needs to be applied
        shouldNotPaginate = Object.keys(queryParams).some(
          (key) => (key !== "isFilter" && key !== "liked") || parsedPublic.isMine
        );
        PIDs = (
          await neo4jdao("READ").run(
            sortedPaginationByLikesCount(
              shouldNotPaginate,
              sortParams.likes,
              { active: queryParams.liked, userId: user._id.toString() },
              pageNumber,
              pageSize
            )
          )
        ).records.map((rec) => ({
          _id: rec?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? undefined,
          likes: rec?.["_fields"]?.[1]?.["low"] ?? 0,
        }));
      }
      const match: any = {};
      if (parsedPublic.isPublic !== undefined) match.isPublic = parsedPublic.isPublic;
      if (!shouldTakeLikes) match._id = { $in: PIDs.map((pid) => new ObjectId(pid._id)) };
      if (parsedPublic?.isMine) match.author = parsedPublic.username;
      if (queryParams?.title) match.title = queryParams.title;
      if (queryParams?.author) match.author = queryParams.author;
      aggregation.push({ $match: match });
      aggregation.push({
        $project: {
          author: 1,
          title: 1,
          creationDate: 1,
          avgDuration: { $round: { $avg: "$songs.duration" } },
        },
      });
      if (queryParams?.year) {
        aggregation.push({
          $project: {
            author: 1,
            title: 1,
            creationDate: 1,
            avgDuration: 1,
            year: { $year: "$creationDate" },
          },
        });
        aggregation.push({ $match: { year: queryParams.year } });
      }
      if (queryParams.averageDuration) {
        aggregation.push({
          $match: {
            $and: [
              { avgDuration: { $gte: queryParams.averageDuration.low } },
              { avgDuration: { $lte: queryParams.averageDuration.high } },
            ],
          },
        });
      }
      if (sortParams.creationDate) {
        aggregation.push({ $sort: { creationDate: sortParams.creationDate } });
      }
      if (sortParams.avgDuration) {
        aggregation.push({ $sort: { avgDuration: sortParams.avgDuration } });
      }
      if (!shouldTakeLikes) {
        if (shouldNotPaginate) {
          // if neo4j should not paginate then mongo will
          aggregation.push({ $skip: pageNumber * pageSize });
          aggregation.push({ $limit: pageSize });
        }
        type EnhancedPlaylist = IPlaylist & { avgDuration?: number; year?: number };
        const playlists: EnhancedPlaylist[] = await Playlist.aggregate(aggregation, {
          allowDiskUse: true,
        });
        const reply: (EnhancedPlaylist & { likesCount?: number })[] = playlists.map((pl) => ({
          title: pl.title,
          author: pl.author,
          creationDate: pl.creationDate,
          _id: pl._id,
          avgDuration: pl.avgDuration,
          likesCount: PIDs?.find((pid) => pid._id === pl._id.toString())?.likes ?? 0,
        }));
        return res.json({
          reply: reply.sort((el1, el2) => sortParams.likes * (el1.likesCount - el2.likesCount)),
        });
      } else {
        aggregation.push({ $skip: pageNumber * pageSize });
        aggregation.push({ $limit: pageSize });
        type EnhancedPlaylist = IPlaylist & { avgDuration?: number; year?: number };
        const playlists: EnhancedPlaylist[] = await Playlist.aggregate(aggregation, {
          allowDiskUse: true,
        });
        const whereCondition =
          (playlists?.length ?? 0) > 0
            ? `where ${playlists.map((pl) => `p._id = "${pl._id.toString()}"`).join(" OR ")}`
            : "";
        PIDs = (
          await neo4jdao("READ").run(
            `MATCH (p:Playlist)<-[l:LIKES]-(user) ${whereCondition} return p, count(l) as likes`
          )
        ).records.map((rec) => ({
          _id: rec?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? undefined,
          likes: rec?.["_fields"]?.[1]?.["low"] ?? 0,
        }));
        const reply: (EnhancedPlaylist & { likesCount: number })[] = playlists.map((pl) => ({
          author: pl.author,
          title: pl.title,
          creationDate: pl.creationDate,
          _id: pl._id,
          likesCount: PIDs?.find((pid) => pid._id === pl._id.toString())?.likes ?? 0,
          avgDuration: pl.avgDuration,
          year: pl.year,
        }));
        return res.status(200).json({ reply });
      }
    } else {
      const aggregation: any[] = [];
      const likeOrdering = (sortParams?.likes ?? undefined) === -1 ? "DESC" : "ASC";
      let PIDs: any = undefined;
      if (queryParams.likeHops) {
        PIDs = (
          await neo4jdao("READ").run(
            likeSuggestion(user._id.toString(), likeOrdering, pageNumber, pageSize)
          )
        ).records.map((rec) => ({
          _id: rec?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? undefined,
          likes: rec?.["_fields"]?.[1]?.["low"] ?? 0,
        }));
      } else if (queryParams.cloneHops) {
        PIDs = (
          await neo4jdao("READ").run(
            hopsSuggestion(user._id.toString(), likeOrdering, pageNumber, pageSize)
          )
        ).records.map((rec) => ({
          _id: rec?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? undefined,
          likes: rec?.["_fields"]?.[1]?.["low"] ?? 0,
        }));
      }
      aggregation.push({ $match: { _id: { $in: PIDs.map((pid) => new ObjectId(pid._id)) } } });
      aggregation.push({
        $project: {
          author: 1,
          title: 1,
          creationDate: 1,
          avgDuration: { $round: { $avg: "$songs.duration" } },
        },
      });
      if (sortParams.avgDuration) {
        aggregation.push({ $sort: { avgDuration: sortParams.avgDuration } });
      }
      if (sortParams.creationDate) {
        aggregation.push({ $sort: { creationDate: sortParams.creationDate } });
      }
      type EnhancedPlaylist = IPlaylist & { avgDuration?: number; year?: number };
      const playlists: EnhancedPlaylist[] = await Playlist.aggregate(aggregation, {
        allowDiskUse: true,
      });
      const reply: (EnhancedPlaylist & { likesCount: number })[] = playlists.map((pl) => ({
        author: pl.author,
        title: pl.title,
        creationDate: pl.creationDate,
        _id: pl._id,
        likesCount: PIDs?.find((pid) => pid._id === pl._id.toString())?.likes ?? 0,
        avgDuration: pl.avgDuration,
      }));
      return res.status(200).json({
        reply: sortParams.likes
          ? reply.sort((el1, el2) => sortParams.likes * (el1.likesCount - el2.likesCount))
          : reply,
      });
    }
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const createPlaylist = async (req: Request, res: Response) => {
  let operation = undefined;
  let newPlaylist: Playlist = undefined;
  try {
    const requestedTitle = req.body?.title ?? "";
    const creator = req.body?.author ?? "";
    const userCreator: IUser = await User.findOne({ username: creator });
    if (!userCreator) throw { code: 404, message: `${creator} does not exist` };
    if (typeof requestedTitle !== "string" || requestedTitle.length <= 0)
      throw { code: 400, message: "invalid title" };
    if (typeof creator !== "string" || requestedTitle.length <= 0)
      throw { code: 400, message: "invalid author" };
    newPlaylist = new Playlist({
      title: requestedTitle,
      author: creator,
      songs: [],
      isPublic: false,
      creationDate: new Date(),
    });
    await newPlaylist.save();
    operation = 1;
    await neo4jdao("WRITE").run(
      `MATCH (u:User{_id: "${userCreator._id.toString()}"})
      CREATE (u)-[:CREATED]->(p:Playlist{_id: "${newPlaylist._id.toString()}"})
      RETURN p`
    );
    return res.status(200).json({
      reply: {
        title: newPlaylist.title,
        author: newPlaylist.author,
        _id: newPlaylist._id.toString(),
        likesCount: 0,
        avgDuration: 0,
      },
    });
  } catch (error) {
    if (operation && newPlaylist) {
      Playlist.deleteOne({ _id: newPlaylist._id });
    }
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const deletePlaylist = async (req: Request, res: Response) => {
  try {
    const PID = req.params.playlistId;
    const toDelete: IPlaylist = await Playlist.findOne({ _id: new ObjectId(PID) });
    if (!toDelete) throw { code: 404, message: "invalid playlist" };
    if (toDelete.isPublic)
      throw { code: errorCodes.NOT_ALLOWED, message: "playlist is already published" };
    await neo4jdao("WRITE").run(
      `MATCH (p:Playlist{_id: "${toDelete._id.toString()}"}) detach delete p`
    );
    await Playlist.deleteOne({ _id: toDelete._id });
    return res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const getPlaylistInfos = async (req: Request, res: Response) => {
  try {
    const { playlistId } = req.params;
    const userRequesting: IUser = untokenize(req.headers["authorization"].split(" ")[1]) as IUser;
    const toReturn: IPlaylist = await Playlist.findOne({ _id: new ObjectId(playlistId) });
    if (!toReturn) throw { code: 404, message: "invalid playlist" };
    let isLiked = false;
    const editedSongs = new EditedSong();
    for (let song of toReturn.songs) {
      const isEdited = await editedSongs.exists(song._id.toString());
      if (isEdited) {
        const editedFields: IEditedSong = (await editedSongs.get(song._id.toString())) as any;
        song.title = editedFields.title;
        song.artist = editedFields.artist;
        song.duration = editedFields.duration;
        song.link = editedFields.link;
      }
    }
    if (toReturn.author !== userRequesting.username) {
      isLiked =
        (
          await neo4jdao("READ").run(
            `MATCH (u:User{_id: "${userRequesting._id.toString()}"}), (p:Playlist{_id: "${playlistId}"})
           RETURN EXISTS((u)-[:LIKES]->(p)) as isLiked`
          )
        ).records?.[0]?.["_fields"]?.[0] ?? false;
    }
    const hasCloned =
      (
        await neo4jdao("READ").run(
          `MATCH (mine:Playlist{_id: "${toReturn._id.toString()}"})-[c:CLONES]->(original) return original`
        )
      ).records?.[0]?.["_fields"]?.[0]?.["properties"]?.["_id"] ?? false;
    let original: IPlaylist = undefined;
    if (hasCloned) {
      const aggregation = [];
      aggregation.push({ $match: { _id: new ObjectId(hasCloned) } });
      aggregation.push({ $project: { author: 1, title: 1 } });
      original = (await Playlist.aggregate(aggregation))[0];
    }
    const nLikes =
      (
        await neo4jdao("READ").run(
          `MATCH (p:Playlist{_id: "${toReturn._id.toString()}"})<-[l:LIKES]-(anyone) return count(l) as likes`
        )
      ).records?.[0]?.["_fields"]?.[0]?.["low"] ?? 0;

    const popScore =
      (await neo4jdao("READ").run(playlistPopularityScore(toReturn._id.toString()))).records?.[0]?.[
        "_fields"
      ]?.[1] ?? 0;

    const reply = {
      ...toReturn,
      isLiked,
      nLikes,
      popScore,
      original,
    };

    return res.status(200).json({ reply });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const updatePlaylist = async (req: Request, res: Response) => {
  try {
    const payload = req.body;
    const { playlistId } = req.params;
    const requestType = req.query.type;
    if (requestType !== "add" && requestType !== "remove")
      throw { code: errorCodes.BAD_REQUEST, message: "invalid operation" };
    const toUpdate: IPlaylist = await Playlist.findOne({ _id: new ObjectId(playlistId) });
    if (!toUpdate) throw { code: 404, message: "invalid playlist" };
    validateSchema(payload, patchPlaylistSchema);
    const playlistToUpdate = new Playlist(toUpdate);
    const toAdd: ISong[] = await Song.find({
      _id: { $in: payload.songsList.map((s) => new ObjectId(s)) },
    });
    const reducedSongs: ReducedSong[] = toAdd.map((s) => ({
      _id: s._id.toString(),
      title: s.title,
      artist: s.artist,
      link: s.link,
      duration: s.duration,
      insertionDate: new Date(),
    }));
    if (requestType === "add") playlistToUpdate.songs.push(...reducedSongs);
    const mappedReducedSongs = reducedSongs.map((rs) => rs._id);
    if (requestType === "remove")
      playlistToUpdate.songs = playlistToUpdate.songs.filter(
        (s) => !mappedReducedSongs.includes(s._id)
      );
    await playlistToUpdate.update();
    return res.status(200).json({ reply: playlistToUpdate });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const getAllPlaylists = async (req: Request, res: Response) => {
  try {
    const { authorId } = req.params;
    const author: IUser = await User.findOne({ _id: new ObjectId(authorId) });
    const personalPlaylists: IPlaylist[] = await Playlist.find({ author: author.username });
    const reply = personalPlaylists.map((pp) => ({
      _id: pp._id.toString(),
      title: pp.title,
      author: pp.author,
    }));
    return res.status(200).json({ reply });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const clonePlaylist = async (req: Request, res: Response) => {
  let operation = 0;
  let newPlaylist: Playlist = undefined;
  try {
    const { playlistId } = req.params;
    const { title } = req.body as { title: string };
    const sessionToken = req.headers["authorization"].split(" ")[1];
    const userData: IUser = untokenize(sessionToken) as any;
    if (!title.length) throw { code: errorCodes.FORBIDDEN, message: "title can not be empty" };
    const originalSongs: ReducedSong[] =
      (
        await Playlist.aggregate([
          { $match: { _id: new ObjectId(playlistId) } },
          { $project: { _id: 0, songs: 1 } },
        ])
      )?.[0]?.songs ?? ([] as ReducedSong[]);
    const editedSongs = new EditedSong();
    for (let song of originalSongs) {
      const isEdited = await editedSongs.exists(song._id.toString());
      if (isEdited) {
        const editedFields: IEditedSong = (await editedSongs.get(song._id.toString())) as any;
        song.title = editedFields.title;
        song.artist = editedFields.artist;
        song.link = editedFields.link;
        song.duration = editedFields.duration;
      }
    }
    newPlaylist = new Playlist({
      title: title,
      author: userData.username,
      songs: originalSongs,
      isPublic: false,
      creationDate: new Date(),
    });
    await newPlaylist.save();
    operation = 1;
    await neo4jdao("WRITE").run(
      `MATCH (p:Playlist{_id: "${playlistId}"}), (u:User{_id: "${userData._id.toString()}"})
      CREATE (p)<-[:CLONES]-(newPl:Playlist{_id: "${newPlaylist._id.toString()}"}), (u)-[:CREATED]->(newPl)
      RETURN newPl`
    );
    return res.status(200).json({ reply: newPlaylist._id.toString() });
  } catch (error) {
    if (operation === 1 && newPlaylist) {
      await Playlist.deleteOne({ _id: newPlaylist._id });
    }
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const likePlaylist = async (req: Request, res: Response) => {
  try {
    const { playlistId } = req.params;
    const sessionToken = req.headers["authorization"].split(" ")[1];
    const userData: IUser = untokenize(sessionToken) as IUser;
    const alreadyLiked =
      (
        await neo4jdao("READ").run(
          `MATCH (u:User{_id: "${userData._id.toString()}"}), (p:Playlist{_id: "${playlistId}"})
       RETURN EXISTS((u)-[:LIKES]->(p)) as exists`
        )
      )?.records?.[0]?.["_fields"]?.[0] ?? false;
    if (alreadyLiked) {
      await neo4jdao("WRITE").run(
        `MATCH (u:User{_id: "${userData._id.toString()}"})-[l:LIKES]->(p:Playlist{_id: "${playlistId}"})
        DELETE l`
      );
    } else {
      await neo4jdao("WRITE").run(
        `MATCH (u:User{_id: "${userData._id.toString()}"}), (p:Playlist{_id: "${playlistId}"})
       CREATE (u)-[:LIKES]->(p)`
      );
    }
    return res.status(200).json({ reply: !alreadyLiked });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const publishPlaylist = async (req: Request, res: Response) => {
  try {
    const { playlistId } = req.params;
    const playlistData: IPlaylist = await Playlist.findOne({ _id: new ObjectId(playlistId) });
    const toPublish = new Playlist(playlistData);
    toPublish.isPublic = true;
    await toPublish.update();
    return res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};
