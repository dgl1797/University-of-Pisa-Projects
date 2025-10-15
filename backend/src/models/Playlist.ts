import { ObjectId } from "mongodb";
import { MongoBase } from "../daos";
import { ISong } from "./Song";

export interface ReducedSong {
  _id: string;
  link: string;
  artist: string;
  title: string;
  duration: number;
  insertionDate: Date;
}

export interface IPlaylist {
  _id?: ObjectId;
  author?: string;
  title?: string;
  songs?: ReducedSong[];
  isPublic?: boolean;
  creationDate?: Date;
}

export class Playlist extends MongoBase<IPlaylist> implements IPlaylist {
  _id: ObjectId;
  author: string;
  title: string;
  songs: ReducedSong[];
  isPublic: boolean;
  creationDate: Date;
  ["constructor"]: typeof Playlist;
  constructor(obj?: IPlaylist) {
    super();
    Object.assign(this, obj);
  }
  static get collectionName(): string {
    return "playlists";
  }
}
