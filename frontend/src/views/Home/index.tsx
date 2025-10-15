import React, { memo } from "react";
import { useSelector } from "react-redux";
import MediaQuery from "react-responsive";
import { useNavigate } from "react-router-dom";
import { Button, Card } from "../../components";
import { selectors } from "../../store/reducers";
import classes from "./home.module.css";
import useHome from "./useHome";

const Home = () => {
  const { statistics } = useHome();
  const navigate = useNavigate();
  const userData = useSelector(selectors.user.getUserData);
  const userToken = useSelector(selectors.session.getSessionToken);

  return (
    <div className={classes.page}>
      <Card title="JA - Playlist Manager">
        {/* example of MediaQuery component usage from react-responsive */}
        <MediaQuery minWidth={930}>
          {/* will be rendered only if the screen is over 930px of width */}
          <div className={classes.actions}>
            <div className={classes.stats}>
              Active Users: <div className={classes.blueText}>{statistics.activeUsers}</div>
            </div>
            <div className={classes.stats}>
              Active Validators:{" "}
              <div className={classes.blueText}>{statistics.activeValidators}</div>
            </div>
            <div className={classes.stats}>
              Playlists created this year:{" "}
              <div className={classes.blueText}>{statistics.numberOfPlaylistsCreatedThisYear}</div>
            </div>
            <div className={classes.stats}>
              Playlists Published:{" "}
              <div className={classes.blueText}>{statistics.numberOfPublishedPlaylist}</div>
            </div>
            <div className={classes.stats}>
              Songs Stored: <div className={classes.blueText}>{statistics.numberOfSongs}</div>
            </div>
          </div>
        </MediaQuery>

        <div className={classes.actions}>
          <Button text="Sign up" onClick={() => navigate("/signup")} />
          <Button text="Log in" onClick={() => navigate("/login")} />
          <Button
            text="Explore"
            onClick={() =>
              navigate(`${userData && userToken ? "/dashboard/rankings" : "/rankings"}`)
            }
          />
        </div>

        <MediaQuery maxWidth={930}>
          <div className={classes.actions}>
            {Object.keys(statistics ?? {}).map((key) => {
              return (
                <div className={`${classes.stats}`}>{`${key}: ${statistics?.[key] ?? ""}`}</div>
              );
            })}
          </div>
        </MediaQuery>
      </Card>
    </div>
  );
};

export default memo(Home);
