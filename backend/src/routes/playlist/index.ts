import express from "express";
import * as handlers from "./handlers";
import * as middlewares from "../middlewares";

const playlistRouter = express.Router();

playlistRouter.get("/rankings", handlers.getPlaylistsByRanking);
playlistRouter.get(
  "/getAll/:authorId",
  middlewares.validateAuthorization,
  handlers.getAllPlaylists
);
playlistRouter.get("/", middlewares.validateAuthorization, handlers.getPlaylists);
playlistRouter.post("/", middlewares.validateAuthorization, handlers.createPlaylist);
playlistRouter.delete("/:playlistId", middlewares.validateAuthorization, handlers.deletePlaylist);
playlistRouter.get("/:playlistId", middlewares.validateAuthorization, handlers.getPlaylistInfos);
playlistRouter.put("/:playlistId", middlewares.validateAuthorization, handlers.updatePlaylist);
playlistRouter.post(
  "/:playlistId/clone",
  middlewares.validateAuthorization,
  handlers.clonePlaylist
);
playlistRouter.post("/:playlistId/like", middlewares.validateAuthorization, handlers.likePlaylist);
playlistRouter.put(
  "/:playlistId/publish",
  middlewares.validateAuthorization,
  handlers.publishPlaylist
);
export default playlistRouter;
