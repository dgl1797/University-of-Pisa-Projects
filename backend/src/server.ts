import express, { Request, Response } from "express";
import cors from "cors";
import path from "path";
const app = express();
app.use(cors());
app.use(express.json());
app.use(express.urlencoded({ extended: true }));
const appRoot = path.resolve(__dirname, "..", "build");
app.use("/app", express.static(appRoot));

/**
 * @ROUTERS
 */
import routers from "./routes";
import { updateIncoherencies, upgradeAccount } from "./cronjobs";
import { getAppInfos } from "./utils";

/**
 * @ROUTES
 */
app.use("/auth", routers.auth);
app.use("/users", routers.user);
app.use("/playlists", routers.playlist);
app.use("/songs", routers.song);
app.get("/infos", getAppInfos);

app.get("/", (req: Request, res: Response) => {
  res.redirect("/app");
});

app.use("/app/*", (req: Request, res: Response) => {
  res.sendFile(path.resolve(__dirname, "..", "build", "index.html"));
});

/**
 * @INITIALIZATIONS
 */

// require("dotenv").config({ path: path.resolve(__dirname, "..", `.env.${process.env.ENV_NAME}`) });

const hostPort = process.env.PORT;
const hostName = process.env.URI;

updateIncoherencies(1000 * 3600 /* 1h */).then(() => {
  console.log("incoherencies caching active");
});

upgradeAccount(1000 * 3600 * 24 /* 1d */).then(() => {
  console.log("account upgrader active");
});

app.listen(hostPort, () => {
  console.log(`Server listening at: http://${hostName}:${hostPort}`);
});
