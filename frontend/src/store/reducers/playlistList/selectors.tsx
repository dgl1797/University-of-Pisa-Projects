import { RootState } from "../..";

export const getPublicList = (state: RootState) => state?.playlistList?.publicList ?? [];
export const getPrivateList = (state: RootState) => state?.playlistList?.privateList ?? [];
export const getPlaylist = (state: RootState) => state?.playlistList?.item ?? undefined;
export const getSelectableList = (state: RootState) =>
  state?.playlistList?.selectablePlaylist ?? [];
