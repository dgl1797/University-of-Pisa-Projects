import express from "express";
import * as handlers from "./handlers";

const authRouter = express.Router();

authRouter.get("/", handlers.getAuth);
authRouter.delete("/", handlers.deleteAuth);

export default authRouter;
