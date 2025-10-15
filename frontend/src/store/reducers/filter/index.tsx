import { createSlice } from "@reduxjs/toolkit";
import { FilterState } from "./interfaces";
import * as selectors from "./selectors";

const initialState: FilterState = { authorFilter: "" };

const filterSlice = createSlice({
  name: "filter",
  initialState,
  reducers: {
    updateAuthorFilter: (state, action: { payload: string }) => {
      state.authorFilter = action.payload;
    },
    //updateUnpersistentCounter: (state, action: { payload: number }) => {
    //  state.unpersistentCounter = action.payload;
    //},
  },
  extraReducers: (builder) => {
    builder.addCase("app/startup", (state) => {
      state.authorFilter = initialState.authorFilter;
    });
  },
});

export default filterSlice;
export { selectors };
