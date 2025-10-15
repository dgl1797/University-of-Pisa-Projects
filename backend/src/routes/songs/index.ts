import express from "express";
import * as handlers from "./handlers";
import * as middlewares from "../middlewares";

const songsRouter = express.Router();

songsRouter.get("/", middlewares.validateAuthorization, handlers.getSongsList);
songsRouter.post("/propose", middlewares.validateAuthorization, handlers.postProposal);
songsRouter.post("/report", middlewares.validateAuthorization, handlers.postReport);
songsRouter.get("/reports/number", middlewares.validateAuthorization, handlers.getReportsNumber);
songsRouter.get("/reports", middlewares.validateAuthorization, handlers.getReports);
songsRouter.post("/", middlewares.validateAuthorization, handlers.postLink);
songsRouter.delete("/reports", middlewares.validateAuthorization, handlers.deleteProposal);
songsRouter.get("/search", middlewares.validateAuthorization, handlers.searchSongs);
songsRouter.put("/link", middlewares.validateAuthorization, handlers.updateLink);
export default songsRouter;
