import express from "express";
import * as handlers from "./handlers";
import * as middlewares from "../middlewares";

const userRouter = express.Router();

userRouter.get("/rankings", handlers.getUsersRank);
userRouter.get("/:userId", middlewares.validateAuthorization, handlers.getUser);
userRouter.put("/:userId", middlewares.validateAuthorization, handlers.updateUser);
userRouter.delete("/:userId", middlewares.validateAuthorization, handlers.deleteUser);

export default userRouter;
