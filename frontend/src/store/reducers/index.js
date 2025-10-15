import * as session from "./session";
import * as user from "./user";
import * as filter from "./filter";
import * as playlistList from "./playlistList";
import * as songList from "./songList";
import * as app from "./app";

export const reducers = {
  session: session.default.reducer,
  user: user.default.reducer,
  playlistFilter: filter.default.reducer,
  playlistList: playlistList.default.reducer,
  songList: songList.default.reducer,
  app: app.default.reducer,
};

export const actions = {
  session: session.default.actions,
  user: user.default.actions,
  playlistFilter: filter.default.actions,
  playlistList: playlistList.default.actions,
  songList: songList.default.actions,
  app: app.default.actions,
};

export const selectors = {
  session: session.selectors,
  user: user.selectors,
  playlistFilter: filter.selectors,
  playlistList: playlistList.selectors,
  songList: songList.selectors,
  app: app.selectors,
};
