import { memo } from "react";
import useRankings from "./useRankings";
import classes from "./rankings.module.css";
import crownIcon from "../../assets/goldenCrown.png";
import { Link } from "react-router-dom";

const Rankings = () => {
  const { userData, playlistRank, userRank, linkClick } = useRankings();
  return playlistRank.length > 0 && userRank.length > 0 ? (
    <div className={classes.page}>
      <div className={classes.container}>
        <div className={`${classes.userRow} ${classes.shadowed}`}>
          <div className={classes.title}>TOP 20 USERS</div>
          <div> </div>
        </div>
        {userRank?.map((u) => (
          <div className={classes.userRow} key={u._id}>
            <div>
              {userData ? (
                <Link to={"/dashboard/playlists"} onClick={() => linkClick(u.username)}>
                  {u.username.toUpperCase()}
                </Link>
              ) : (
                u.username.toUpperCase()
              )}
            </div>
            <div className={classes.score}>
              <img
                src={crownIcon}
                alt="score"
                style={{ marginRight: "3px" }}
                width={20}
                height={20}
              />
              {u.score}
            </div>
          </div>
        )) ?? null}
      </div>
      <div className={classes.container}>
        <div className={`${classes.playlistsRow} ${classes.shadowed}`}>
          <div className={classes.title}>TOP 20 PLAYLISTS</div>
          <div className={classes.title}>BY</div>
          <div> </div>
        </div>
        {playlistRank?.map((p) => (
          <div className={classes.playlistsRow} key={p._id}>
            <div>
              {userData ? (
                <Link to={`/dashboard/playlists/${p._id}`}>{p.title.toUpperCase()}</Link>
              ) : (
                p.title.toUpperCase()
              )}
            </div>
            <div>
              {userData ? (
                <Link to={"/dashboard/playlists"} onClick={() => linkClick(p.author)}>
                  {p.author.toUpperCase()}
                </Link>
              ) : (
                p.author.toUpperCase()
              )}
            </div>
            <div className={classes.score}>
              <img
                src={crownIcon}
                style={{ marginRight: "3px" }}
                alt="score"
                width={20}
                height={20}
              />
              {p.score}
            </div>
          </div>
        )) ?? null}
      </div>
    </div>
  ) : (
    <div className={classes.loadingPage}>Loading...</div>
  );
};

export default memo(Rankings);
