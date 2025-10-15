import React, { memo } from "react";
import classes from "./extracss.module.css";
import mainClasses from "../view.module.css";
import { Card } from "../../../components";
import usePlaylistModal from "./usePlaylistModal";

type PlaylistModalProps = {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (...args: any) => void;
  selectedSongs: string[];
};

const PlaylistsModal = ({ isOpen, onClose, onSubmit, selectedSongs }: PlaylistModalProps) => {
  const { error, successMessage, personalPlaylists, mergeSongs } = usePlaylistModal(onClose);
  return (
    <div className={`${isOpen ? classes.open : classes.closed}`}>
      {error && (
        <div className={mainClasses.feedback}>
          <div className={mainClasses.message}>{error}</div>
        </div>
      )}
      {successMessage && (
        <div className={mainClasses.feedback}>
          <div className={mainClasses.smessage}>{successMessage}</div>
        </div>
      )}
      <Card title="SELECT PLAYLIST" className={classes.modalWindow}>
        <div className={classes.closeButton} onClick={onClose}>
          ✘
        </div>
        <div className={classes.listContainer}>
          {personalPlaylists.map((pp) => (
            <div
              className={classes.plList}
              onClick={() => mergeSongs(pp._id, selectedSongs)}
              key={pp._id}
              style={{ textAlign: "center" }}
            >
              {pp.title}
            </div>
          ))}
        </div>
      </Card>
    </div>
  );
};

export default memo(PlaylistsModal);
