import moment from "moment";
import { ObjectId } from "mongodb";
import { mongodao, neo4jdao, redisdao } from "../daos";
import { EditedSong, IEditedSong, Interval, IPlaylist, IUser, Session, User } from "../models";

export const updateIncoherencies = async (retryTime: number) => {
  const intervals = new Interval();
  const exists = await intervals.exists("incoherencies");
  if (exists) return;
  const intervalId: NodeJS.Timer = setInterval(
    () => setTimeout(updateIncoherenciesWorker(intervalId), 0),
    retryTime
  );
};
const updateIncoherenciesWorker = (intervalId: NodeJS.Timer) => async () => {
  try {
    const intervals = new Interval();
    await intervals.set("incoherencies");
    const editedSongs = new EditedSong();
    const returnedKeys = await redisdao().keys(`${Session.prefix}:*`);
    const { db, instance } = await mongodao();
    const totalUsers = await db.collection("users").countDocuments();
    if (returnedKeys.length < (30 / 100) * totalUsers) {
      const editedIds = await redisdao().keys(`${EditedSong.prefix}:*`);
      if (!editedIds.length) {
        await intervals.delete("incoherencies");
        clearInterval(intervalId);
        return;
      }
      const { db, instance } = await mongodao();
      for (let id of editedIds.map((eid) => eid.split(":")[1])) {
        const updatedData: IEditedSong = (await editedSongs.get(id)) as any;
        const toUpdate: IPlaylist[] = await db
          .collection("playlists")
          .find({ "songs._id": id })
          .toArray();
        for (let pl of toUpdate) {
          pl.songs = pl.songs.map((s) => {
            if (s._id === id) {
              return {
                ...s,
                title: updatedData.title,
                artist: updatedData.artist,
                duration: updatedData.duration,
                link: updatedData.link,
              };
            }
            return s;
          });
          await db.collection("playlists").updateOne({ _id: pl._id }, { $set: { ...pl } });
        }
        await editedSongs.delete(id);
      }
      instance.close();
    }
  } catch (error) {
    console.error("error");
  }
};

export const upgradeAccount = async (retryTime: number) => {
  const intervalId: NodeJS.Timer = setInterval(
    () => setTimeout(upgradeAccountWorker(intervalId), 0),
    retryTime
  );
};

const topUserByPopularityScore = `call{
  match (u:User{isValidator: 1})-[:CREATED]->(p:Playlist)<-[l:LIKES]-(tizio)
  return u, p, 0.5*count(l) as score
  union
  match (u:User{isValidator: 1})-[:CREATED]->(p:Playlist)<-[c:CLONES]-(p2)
  return u, p, count(c) as score
}
return u, sum(score) as score
ORDER BY score DESC LIMIT 1`;

const upgradeAccountWorker = (intervalId: NodeJS.Timer) => async () => {
  try {
    const month = parseInt(moment().format("MM"));
    const day = parseInt(moment().format("DD"));
    if (month === 1 && day === 1) {
      const topCommonUser =
        (await neo4jdao("READ").run(topUserByPopularityScore))?.records?.[0]?.["_fields"]?.[0]?.[
          "properties"
        ]?.["_id"] ?? undefined;
      await User.updateOne({ _id: new ObjectId(topCommonUser) }, { isValidator: true });
      await neo4jdao("WRITE").run(`MATCH (u:User{_id: "${topCommonUser}"}) set u.isValidator: 1`);
    }
  } catch (error) {
    console.error("error");
  }
};
