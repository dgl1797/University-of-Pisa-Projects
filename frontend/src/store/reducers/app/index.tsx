import { createSlice } from "@reduxjs/toolkit";
import { AppInfoState } from "./interfaces";
import * as selectors from "./selectors";

const initialState: AppInfoState = {
  info: {
    activeUsers: 0,
    activeValidators: 0,
    numberOfPlaylistsCreatedThisYear: 0,
    numberOfPublishedPlaylist: 0,
    numberOfSongs: 0,
  },
};

const appSlice = createSlice({
  name: "app",
  initialState,
  reducers: {
    setAppInfo: (state, action: { payload: any }) => {
      state.info = action.payload;
    },
  },
});

export default appSlice;
export { selectors };
