export interface IUser {
  username: string;
  _id: string;
  isValidator: boolean;
}

export interface IUserRank {
  username: string;
  _id: string;
  score: number;
}
