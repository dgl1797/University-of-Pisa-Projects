import { createSlice } from "@reduxjs/toolkit";
import * as selectors from "./selectors";

const initialState: { token: string | undefined } = {
  token: undefined,
};

const sessionSlice = createSlice({
  name: "session",
  initialState,
  reducers: {
    setSession: (state, action: { payload: { token: string | undefined } }) => {
      state.token = action.payload.token;
    },
    deleteSession: (state) => {
      state.token = undefined;
    },
  },
});

export { selectors };
export default sessionSlice;
