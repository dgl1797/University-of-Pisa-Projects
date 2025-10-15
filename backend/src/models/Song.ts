import { ObjectId } from "mongodb";
import { MongoBase, RedisBase } from "../daos";

export interface Report {
  isProposal: boolean;
  lastReportDate: Date;
}

export interface ISong {
  _id?: ObjectId;
  link?: string;
  artist?: string;
  title?: string;
  duration?: number;
  reports?: Report;
}

export class Song extends MongoBase<ISong> implements ISong {
  _id: ObjectId;
  link?: string;
  artist?: string;
  title?: string;
  duration?: number;
  reports?: Report;
  ["constructor"]: typeof Song;
  constructor(obj?: ISong) {
    super();
    Object.assign(this, obj);
  }
  static get collectionName(): string {
    return "songs";
  }
}

export interface IEditedSong {
  title: string;
  artist: string;
  duration: number;
  link: string;
}

export class EditedSong extends RedisBase implements IEditedSong {
  artist: string;
  title: string;
  link: string;
  duration: number;
  ["constructor"]: typeof EditedSong;
  constructor(obj?: IEditedSong) {
    super();
    Object.assign(this, obj);
  }
  static get prefix(): string {
    return "edited_songs";
  }
}
