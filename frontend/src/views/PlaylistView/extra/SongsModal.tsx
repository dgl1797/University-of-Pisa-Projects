import React, { memo } from "react";
import { Button, Card, Input } from "../../../components";
import classes from "./extracss.module.css";
import mainClasses from "../view.module.css";
import useSongsModal from "./useSongsModal";

type SongsModalProps = {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (...args: any) => void;
};

const SongsModal = ({ isOpen, onClose, onSubmit }: SongsModalProps) => {
  const {
    handleDynamicSearch,
    titleRef,
    artistRef,
    songsList,
    hasMore,
    loaderRef,
    selectAll,
    selectedSongs,
    handleSelection,
    nose,
    isLoading,
    handleConfirmation,
    error,
    proposedLink,
    proposalBar,
    viewProposalBar,
    handleProposal,
    successMessage,
  } = useSongsModal(onClose);
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
      <Card className={classes.modalWindow}>
        <div className={classes.closeButton} onClick={onClose}>
          ✘
        </div>
        <div className={classes.searchBars}>
          <div> </div>
          <Input
            inputType={"text"}
            label="SEARCH"
            className={`${mainClasses.inputBar} ${classes.lowerLabel}`}
            reference={titleRef}
            onChange={() => {
              viewProposalBar();
              handleDynamicSearch();
            }}
          />
          <Input
            inputType={"text"}
            label="SEARCH"
            className={`${mainClasses.inputBar} ${classes.lowerLabel}`}
            reference={artistRef}
            onChange={() => {
              viewProposalBar();
              handleDynamicSearch();
            }}
          />
        </div>
        <div className={`${classes.nose} ${!nose ? classes.hidden : ""}`}>⅃</div>
        <div className={proposalBar ? classes.proposal : classes.hidden}>
          <Input label="PROPOSE LINK" inputType={"text"} reference={proposedLink} />
        </div>
        <div className={classes.songList}>
          <div className={classes.songsHeaders}>
            <input
              type={"checkbox"}
              onChange={(ev) => selectAll(ev.target.checked)}
              className={mainClasses.checkbox}
            />
            <div>TITLE</div>
            <div>ARTIST</div>
          </div>
          <div className={classes.songsContainer}>
            {songsList.map((s) => (
              <div className={classes.songCard} key={s._id}>
                <div
                  onClick={() => window.open(s.link, "_blank")}
                  className={classes.cardClicker}
                ></div>
                <input
                  type={"checkbox"}
                  value={s._id}
                  checked={selectedSongs.includes(s._id)}
                  onChange={(ev) => handleSelection(ev.target.checked, s._id)}
                  className={mainClasses.checkbox}
                />
                <div>{s.title}</div>
                <div>{s.artist}</div>
              </div>
            ))}
            {hasMore && <div ref={loaderRef}>Loading...</div>}
          </div>
          <div style={{ display: "flex", justifyContent: "center", alignItems: "center" }}>
            <Button
              text={`CONFIRM (${selectedSongs.length})`}
              className={classes.confirmBtn}
              disabled={isLoading}
              onClick={() => {
                if (!proposalBar || selectedSongs.length) handleConfirmation();
                else handleProposal();
              }}
            />
          </div>
        </div>
      </Card>
    </div>
  );
};

export default memo(SongsModal);
