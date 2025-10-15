import { Request, Response } from "express";
import { EditedSong, ISong, IUser, Song } from "../../models";
import { transformError, untokenize } from "../../utils";
import errorCodes from "../../configs/errorCodes.json";
import { mongodao, neo4jdao } from "../../daos";
import { updateIncoherencies } from "../../cronjobs";
const ytMusic = require("node-youtube-music");

export const getSongsList = async (req: Request, res: Response) => {
  try {
    const searchString = (req?.query?.q as string) ?? undefined;
    const page = parseInt((req?.query?.page as string) ?? "0");
    const parsedSearch: { title: string; artist: string } = JSON.parse(searchString ?? "{}");
    const title = parsedSearch?.title?.length ? `"${parsedSearch.title}"` : undefined;
    const artist = parsedSearch?.artist?.length ? `"${parsedSearch.artist}"` : undefined;
    const searchStrings = [title, artist].join(" ").trim();
    const songsList: any = (await Song.aggregate([
      {
        $match: {
          $text: { $search: searchStrings, $caseSensitive: false },
          "reports.isProposal": false,
        },
      },
      {
        $match: {
          artist: { $regex: artist?.replace(/"/g, "")?.toLowerCase() ?? "", $options: "i" },
          title: { $regex: title?.replace(/"/g, "")?.toLowerCase() ?? "", $options: "i" },
        },
      },
      {
        $facet: {
          songData: [
            {
              $skip: page * 20,
            },
            {
              $limit: 20,
            },
            {
              $project: {
                title: 1,
                artist: 1,
                _id: 1,
                link: 1,
              },
            },
          ],
          querySize: [{ $count: "querySize" }],
        },
      },
    ])) as any;
    const { songData, querySize } = songsList?.[0] ?? undefined;
    const reply = songData;
    const totalLength = querySize?.[0]?.querySize ?? 0;
    return res.status(200).json({ reply, totalLength });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const postProposal = async (req: Request, res: Response) => {
  let operation = undefined;
  let proposal: Song = undefined;
  try {
    const pLink = req?.query.pl ?? "";
    const sessionToken = req?.headers?.["authorization"]?.split(" ")?.[1] ?? "";
    const userData: IUser = untokenize(sessionToken) as any;
    if (!pLink.length) throw { code: 400, message: "invalid link" };
    const songResult = await ytMusic.searchMusics(pLink as string);
    if (!songResult.length) throw { code: 404, message: "link not found" };
    if (songResult) {
      const foundSong = songResult[0];
      const appLink = `https://youtube.com/watch?v=${foundSong.youtubeId}`;
      const exists: ISong = await Song.findOne({ link: appLink });
      if (exists) {
        if (exists.reports.isProposal) {
          const alreadyReported =
            (
              await neo4jdao("READ").run(
                `MATCH (u:User{_id: "${userData._id.toString()}"}), (s:Song{link: "${appLink}"})
             RETURN EXISTS((u)-[:REPORTED]->(s)) as exists`
              )
            )?.records?.[0]?.["_fields"]?.[0] ?? false;
          if (alreadyReported)
            throw { code: errorCodes.ANAUTHORIZED, message: "you already reported that link" };
          const proposal = new Song(exists);
          proposal.reports.lastReportDate = new Date();
          await proposal.update();
          operation = "updated";
          neo4jdao("WRITE").run(
            `MATCH (u:User{_id: "${userData._id.toString()}"}), (s:Song{link: "${appLink}"}) CREATE (u)-[:REPORTED]->(s)`
          );
          return res.status(200).json({
            message: `The proposal has been succesfully added to the existing ones`,
          });
        } else
          return res.status(200).json({
            reply: [
              {
                _id: exists._id.toString(),
                artist: exists.artist,
                title: exists.title,
                link: exists.link,
              },
            ],
          });
      } else {
        const proposal: Song = new Song({
          title: foundSong.title,
          artist: foundSong.artists.map((a) => a.name).join(" "),
          link: appLink,
          duration: foundSong.duration.totalSeconds,
          reports: {
            isProposal: true,
            lastReportDate: new Date(),
          },
        });
        await proposal.save();
        operation = "saved";
        await neo4jdao("WRITE").run(
          `MATCH (u:User{_id: "${userData._id.toString()}"}) CREATE (u)-[:REPORTED]->(s:Song{link: "${appLink}"})`
        );
        return res.status(200).json({ message: "the proposal has been sucesfully created" });
      }
    }
    return res.json({ songResult });
  } catch (error) {
    if (operation === "updated" && proposal) {
      try {
        await proposal.update();
      } catch (err) {
        const { code, message } = transformError(err);
        return res.status(code).json({ message });
      }
    } else if (operation === "saved" && proposal) {
      try {
        await Song.deleteOne({ _id: proposal._id });
      } catch (err) {
        const { code, message } = transformError(error);
        return res.status(code).json({ message });
      }
    }
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const postReport = async (req: Request, res: Response) => {
  try {
    const repLinks = req?.body?.repLinks ?? undefined;
    const sessionToken = req?.headers?.["authorization"]?.split(" ")?.[1];
    const userData: IUser = untokenize(sessionToken) as any;
    if (!repLinks) throw { code: errorCodes.BAD_REQUEST, message: "invalid link list" };
    let neo4jWhere = repLinks.length
      ? ` where ${repLinks.map((rl) => `s.link="${rl}"`).join(" OR ")}`
      : "";
    const alreadyReportedLinks = (
      await neo4jdao("READ").run(
        `MATCH (u:User{_id: "${userData._id.toString()}"})-[:REPORTED]->(s:Song)${neo4jWhere}
       RETURN s.link`
      )
    ).records.map((rec) => rec?.["_fields"]?.[0] ?? undefined);
    const toReport = repLinks.filter((rl) => !alreadyReportedLinks.includes(rl));
    const mongoSongsToReport: ISong[] = await Song.find({
      link: { $in: toReport },
      "reports.isProposal": false,
    });
    neo4jWhere = mongoSongsToReport.length
      ? ` where ${mongoSongsToReport.map((rl) => `s.link="${rl}"`).join(" OR ")}`
      : "";
    const pidsLimit = 50;
    const processes = [];
    for (let songlink of mongoSongsToReport.map((ms) => ms.link)) {
      if (processes.length < pidsLimit) {
        processes.push(
          neo4jdao("WRITE").run(
            `MATCH (u:User{_id: "${userData._id.toString()}"})
            MERGE (s:Song{link: "${songlink}"})
            MERGE (u)-[:REPORTED]->(s)`
          )
        );
      } else {
        await processes.pop();
        processes.push(
          neo4jdao("WRITE").run(
            `MATCH (u:User{_id: "${userData._id.toString()}"})
            MERGE (s:Song{link: "${songlink}"})
            MERGE (u)-[:REPORTED]->(s)`
          )
        );
      }
    }
    await Song.updateMany(
      { link: { $in: mongoSongsToReport.map((ms) => ms.link) } },
      { $set: { "reports.lastReportDate": new Date() } }
    );
    return res.status(200).json({ message: `Completed ${mongoSongsToReport.length} reports` });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const getReportsNumber = async (req: Request, res: Response) => {
  try {
    const numberOfReports =
      (await neo4jdao("READ").run(`MATCH (s:Song)<-[r:REPORTED]-(anyone) RETURN count(r) as nReps`))
        ?.records?.[0]?.["_fields"]?.[0]?.["low"] ?? 0;
    return res.json({ numberOfReports });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const getReports = async (req: Request, res: Response) => {
  try {
    let ordering = JSON.parse((req?.query?.orderByReps ?? "{}") as string);
    if (typeof ordering === "object") ordering = undefined;

    let isReport = JSON.parse((req?.query?.isReport ?? "{}") as string);
    if (typeof isReport === "object") isReport = false;
    const reportedLinks = (
      await neo4jdao("READ").run(
        `MATCH (s:Song)<-[r:REPORTED]-(anyone) return s.link, count(r) as nReps`
      )
    )?.records?.map((rec) => ({
      link: rec?.["_fields"]?.[0] ?? undefined,
      nReps: rec?.["_fields"]?.[1]?.["low"] ?? undefined,
    }));
    const mongoSongs: ISong[] = await Song.aggregate([
      {
        $match: {
          link: { $in: reportedLinks.map((rl) => rl.link) },
          "reports.isProposal": !isReport,
        },
      },
      {
        $sort: {
          "reports.lastReportDate": -1,
        },
      },
    ]);
    const reply: (ISong & { nReps: number })[] = mongoSongs.map((ms) => ({
      ...ms,
      nReps: reportedLinks.find((rl) => rl.link === ms.link).nReps,
    }));
    return res.status(200).json({
      reply:
        ordering !== undefined
          ? reply.sort((el1, el2) => ordering * (el1.nReps - el2.nReps))
          : reply,
    });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const postLink = async (req: Request, res: Response) => {
  try {
    const link = req?.query?.link ?? "";
    const songData: ISong = await Song.findOne({ link: link });
    const song = new Song(songData);
    song.reports.isProposal = false;
    song.reports.lastReportDate = null;
    await neo4jdao("WRITE").run(`MATCH (s:Song{link: "${link}"}) detach delete s`);
    await song.update();
    return res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const deleteProposal = async (req: Request, res: Response) => {
  try {
    const link = req?.query?.link ?? "";
    await neo4jdao("WRITE").run(`MATCH (s:Song{link: "${link}"}) detach delete s`);
    return res.status(200).json({ message: "OK" });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const searchSongs = async (req: Request, res: Response) => {
  try {
    const songArtist = req?.query?.songArtist ?? undefined;
    const songTitle = req?.query?.songTitle ?? undefined;
    const searchResults: ISong = (
      await ytMusic.searchMusics([songArtist, songTitle].join(" "))
    ).map((res) => ({
      artist: res?.artists?.map((art) => art?.name)?.join(" / ") ?? "",
      duration: res?.duration?.totalSeconds ?? 0,
      title: res?.title,
      link: `https://youtube.com/watch?v=${res.youtubeId}`,
    }));
    return res.json({ reply: searchResults });
  } catch (error) {
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};

export const updateLink = async (req: Request, res: Response) => {
  let operation = undefined;
  let editedSong: EditedSong = undefined;
  let key = undefined;
  try {
    const { songLink, alternativeData } = req.body;
    const songData: ISong = await Song.findOne({ link: songLink });
    if (songData.link === alternativeData.link)
      throw { code: errorCodes.BAD_REQUEST, message: "the links are the same" };
    if (!songData) throw { code: 404, message: "invalid link" };
    editedSong = new EditedSong({
      title: alternativeData.title,
      link: alternativeData.link,
      artist: alternativeData.artist,
      duration: alternativeData.duration,
    });
    key = songData._id.toString();
    await editedSong.set(key);
    operation = "redis";
    await neo4jdao("WRITE").run(`MATCH (s:Song{link: "${songData.link}"}) detach delete s`);
    operation = "neo4j";
    const mongoSong = new Song(songData);
    mongoSong.title = alternativeData.title;
    mongoSong.link = alternativeData.link;
    mongoSong.artist = alternativeData.artist;
    mongoSong.duration = alternativeData.duration;
    await mongoSong.update();
    updateIncoherencies(3600000 /* 1h */);
    return res.status(200).json({ message: "OK" });
  } catch (error) {
    if (operation === "redis" && editedSong && key) {
      try {
        await editedSong.delete(key);
      } catch (err) {
        const { code, message } = transformError(error);
        return res.status(code).json({ message });
      }
    }
    const { code, message } = transformError(error);
    return res.status(code).json({ message });
  }
};
