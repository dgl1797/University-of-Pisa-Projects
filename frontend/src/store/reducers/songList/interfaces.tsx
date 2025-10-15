import { IPlaylist, IPlaylistList, MinimalSong } from "../../models";

export interface ListState {
  list: MinimalSong[];
  totalLength: number;
  proposals: (MinimalSong & { nReps: number })[];
  reported: (MinimalSong & { nReps: number })[];
}
