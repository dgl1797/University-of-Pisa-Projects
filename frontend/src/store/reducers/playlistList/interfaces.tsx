import { IPlaylist, IPlaylistList, ISelectablePlaylist } from "../../models";

export interface ListState {
  publicList: IPlaylistList[];
  privateList: IPlaylistList[];
  selectablePlaylist: ISelectablePlaylist[];
  item: IPlaylist | undefined;
}
