import { ObjectId } from "mongodb";
import { Playlist } from ".";
import { MongoBase, RedisBase } from "../daos";

export interface IUser {
  _id?: ObjectId;
  username?: string;
  password?: string;
  salt?: string;
  isValidator?: boolean;
}

export class User extends MongoBase<IUser> implements IUser {
  _id: ObjectId;
  username: string;
  password: string;
  salt: string;
  isValidator: boolean;
  ["constructor"]: typeof User;
  constructor(obj?: IUser) {
    super();
    Object.assign(this, obj);
  }
  static get collectionName(): string {
    return "users";
  }
}

export interface ISession {
  token: string;
}

export class Session extends RedisBase implements ISession {
  token: string;
  ["constructor"]: typeof Session;
  constructor(token?: string) {
    super();
    this.token = token ?? undefined;
  }
  static get prefix(): string {
    return "session";
  }
}
