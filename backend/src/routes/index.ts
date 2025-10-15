import authRouter from "./auth";
import userRouter from "./user";
import playlistRouter from "./playlist";
import songsRouter from "./songs";

const routers = {
  auth: authRouter,
  user: userRouter,
  playlist: playlistRouter,
  song: songsRouter,
};

export default routers;
