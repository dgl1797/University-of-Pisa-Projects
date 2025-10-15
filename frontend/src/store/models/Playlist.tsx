import { ISong } from ".";

export interface IPlaylistRank {
  title: string;
  _id: string;
  author: string;
  score: number;
}

export interface IPlaylistList {
  title: string;
  _id: string;
  author: string;
  likesCount: number;
  avgDuration: number;
}

export interface ISelectablePlaylist {
  title: string;
  _id: string;
  author: string;
}

export interface IPlaylist {
  _id: string;
  author: string;
  title: string;
  songs: ISong[];
  isPublic: boolean;
  creationDate: string;
  isLiked: boolean;
  nLikes: number;
  popScore: number;
  original?: {
    title: string;
    _id: string;
    author: string;
  };
}
