import React, { memo } from "react";
import { Outlet } from "react-router-dom";
import classes from "./dashboard.module.css";
import playlistIcon from "../../assets/whitePlayIcon.jpg";
import { DropMenu, Tooltip } from "../../components";
import useDashboard from "./useDashboard";

const Dashboard = () => {
  const {
    imageRef,
    changeImage,
    returnImage,
    isOpen,
    close,
    open,
    requestLogout,
    menuOptions,
    navigator,
  } = useDashboard();
  return (
    <>
      <div className={classes.topBar}>
        <div className={classes.barButton} onClick={requestLogout}>
          LOGOUT
        </div>
        {playlistIcon && (
          <Tooltip text="playlists">
            <img
              ref={imageRef}
              src={playlistIcon}
              alt=""
              width={40}
              height={40}
              className={classes.playlistButton}
              onMouseOver={changeImage}
              onMouseOut={returnImage}
              onClick={() => {
                navigator("playlists");
              }}
            />
          </Tooltip>
        )}
        <DropMenu options={menuOptions} isOpen={isOpen} close={close} width={300} height={300}>
          <div className={classes.barButton} onClick={open}>
            PROFILE
          </div>
        </DropMenu>
      </div>
      <div onClick={close}>
        <Outlet />
      </div>
    </>
  );
};

export default memo(Dashboard);
