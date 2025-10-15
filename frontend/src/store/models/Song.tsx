export interface ISong {
  _id: string;
  link: string;
  artist: string;
  title: string;
  duration: number;
  insertionDate: string;
}

export interface MinimalSong {
  _id: string;
  title: string;
  link: string;
  artist: string;
}
