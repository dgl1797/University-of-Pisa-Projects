import { createSlice } from "@reduxjs/toolkit";
import { IUser } from "../../models";
import jwt from "jsonwebtoken";
import * as selectors from "./selectors";
import * as interfaces from "./interfaces";

const initialState: { item: IUser | undefined; info: interfaces.StatState | undefined } = {
  item: undefined,
  info: undefined,
};

const userSlice = createSlice({
  name: "user",
  initialState,
  reducers: {
    setStats: (state, action: { payload: interfaces.StatState }) => {
      state.info = action.payload;
    },
    setItem: (state, action: { payload: IUser }) => {
      state.item = action?.payload ?? state.item;
    },
    deleteItem: (state) => {
      state.item = undefined;
    },
  },
  extraReducers: (builder) => {
    builder.addCase("session/setSession", (state, action: any) => {
      state.item = jwt.decode(action.payload.token) as any;
    });
  },
});

export { selectors };
export default userSlice;
