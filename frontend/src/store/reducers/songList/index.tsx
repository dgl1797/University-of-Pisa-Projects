import { createSlice } from "@reduxjs/toolkit";
import { MinimalSong } from "../../models";
import { ListState } from "./interfaces";
import * as selectors from "./selectors";

const initialState: ListState = {
  list: [],
  totalLength: 0,
  proposals: [],
  reported: [],
};

const playlistListSlice = createSlice({
  name: "slist",
  initialState,
  reducers: {
    setList: (state, action: { payload: MinimalSong[] }) => {
      state.list = action.payload;
    },
    setTotalLength: (state, action: { payload: number }) => {
      state.totalLength = action.payload;
    },
    addToList: (state, action: { payload: MinimalSong[] }) => {
      state.list = state.list.concat(action.payload);
    },
    setProposals: (state, action: { payload: (MinimalSong & { nReps: number })[] }) => {
      state.proposals = action.payload;
    },
    setReports: (state, action: { payload: (MinimalSong & { nReps: number })[] }) => {
      state.reported = action.payload;
    },
    deleteReportProposal: (state, action: { payload: string }) => {
      state.proposals = state.proposals.filter((ps) => ps.link !== action.payload);
      state.reported = state.reported.filter((rs) => rs.link !== action.payload);
    },
  },
  extraReducers: (builder) => {
    builder.addCase("app/startup", (state) => {
      state.list = initialState.list;
      state.totalLength = 0;
    });
  },
});

export default playlistListSlice;
export { selectors };
