import { createSlice } from "@reduxjs/toolkit";
import { IPlaylist, IPlaylistList, ISelectablePlaylist, ISong } from "../../models";
import { ListState } from "./interfaces";
import * as selectors from "./selectors";

const initialState: ListState = {
  publicList: [],
  privateList: [],
  selectablePlaylist: [],
  item: undefined,
};

const playlistListSlice = createSlice({
  name: "plist",
  initialState,
  reducers: {
    setPublicList: (state, action: { payload: IPlaylistList[] }) => {
      state.publicList = action.payload;
    },
    setPrivateList: (state, action: { payload: IPlaylistList[] }) => {
      state.privateList = action.payload;
    },
    addToPrivateList: (state, action: { payload: IPlaylistList }) => {
      state.privateList.push(action.payload);
    },
    setPlaylistInfo: (state, action: { payload: IPlaylist }) => {
      state.item = action.payload;
    },
    updatePlaylistInfo: (state, action: { payload: IPlaylist }) => {
      state.item = {
        ...state.item,
        ...action.payload,
      };
    },
    updatePlaylistList: (state, action: { payload: IPlaylistList }) => {
      state.privateList = state.privateList.map((pp) => ({
        ...pp,
        ...action.payload,
      }));
    },
    setSelectablePlaylists: (state, action: { payload: ISelectablePlaylist[] }) => {
      state.selectablePlaylist = action.payload;
    },
    addSong: (state, action: { payload: ISong }) => {
      const oldData: IPlaylist = { ...state.item } as IPlaylist;
      oldData.songs = (state.item?.songs ?? []).concat(action.payload);
      state.item = oldData;
    },
    removeSong: (state, action: { payload: ISong }) => {
      const oldData: IPlaylist = { ...state.item } as IPlaylist;
      oldData.songs = (state.item?.songs ?? []).filter((s) => s.link !== action.payload._id);
      state.item = oldData;
    },
  },
});

export default playlistListSlice;
export { selectors };
