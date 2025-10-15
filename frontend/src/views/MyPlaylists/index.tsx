import React, { memo } from "react";
import { Button, Card } from "../../components";
import { Filters, PlaylistModal } from "./extra";
import { secondsToDuration } from "../../utils";
import classes from "./myplaylists.module.css";
import useMyPlaylists from "./useMyPlaylists";

const MyPlaylists = () => {
  const {
    title,
    min,
    max,
    year,
    checkOrderingFields,
    likes,
    avgDuration,
    creationDate,
    page,
    updatePage,
    handleSearch,
    error,
    navigate,
    playlistList,
    updatePublic,
    isPublic,
    modalOpen,
    handleModalState,
    handleModalSubmit,
  } = useMyPlaylists();

  return (
    <div className={classes.page}>
      <PlaylistModal
        isOpen={modalOpen}
        onClose={() => handleModalState(false)}
        onSubmit={handleModalSubmit}
      />
      {error && (
        <div className={classes.feedback}>{<div className={classes.message}>{error}</div>}</div>
      )}
      <Card className={classes.filterCard}>
        {" "}
        <div className={`${classes.header} ${classes.tabbed}`}>
          <div className={`${classes.tab} ${classes.activeTab}`}>FILTERS</div>
        </div>
        <Filters
          isPublic={isPublic}
          updatePublic={updatePublic}
          min={min}
          max={max}
          title={title}
          year={year}
        />
      </Card>
      <Card className={classes.listCard}>
        <div className={`${classes.header} ${classes.listHeader}`}>PLAYLISTS</div>
        <div className={classes.pagination}>{page}</div>
        <div className={classes.leftPageArrow} onClick={() => updatePage(page - 1)}>
          🢘
        </div>
        <div className={classes.rightPageArrow} onClick={() => updatePage(page + 1)}>
          🢚
        </div>
        <div className={classes.listContainer}>
          {playlistList.map((pl) => (
            <Card className={classes.plCard} key={pl._id} onClick={() => navigate(`${pl._id}`)}>
              <div className={classes.plElement}>{pl.title}</div>
              <div className={classes.plElement}>by: {pl.author}</div>
              <div className={`${classes.plElement} ${classes.lowerText}`}>
                average songs duration: {secondsToDuration(pl.avgDuration)}
              </div>
              <div className={`${classes.plElement} ${classes.lowerText}`}>👍 {pl.likesCount}</div>
            </Card>
          ))}
        </div>
      </Card>
      <Card className={classes.sortCard}>
        <div className={classes.header}>SORTINGS</div>
        <div className={classes.sortContainer}>
          <button
            className={likes ? classes.active : ""}
            onClick={() => checkOrderingFields(likes === 1 ? -1 : 1)}
          >
            LIKES {`${likes ? (likes === -1 ? "🡩" : "🡫") : ""}`}
          </button>
          <button
            className={avgDuration ? classes.active : ""}
            onClick={() => checkOrderingFields(undefined, avgDuration === 1 ? -1 : 1)}
          >
            DURATION {`${avgDuration ? (avgDuration === -1 ? "🡩" : "🡫") : ""}`}
          </button>
          <button
            className={creationDate ? classes.active : ""}
            onClick={() => checkOrderingFields(undefined, undefined, creationDate === 1 ? -1 : 1)}
          >
            CREATION {`${creationDate ? (creationDate === -1 ? "🡩" : "🡫") : ""}`}
          </button>
          <Button
            text="UNDO"
            onClick={() => checkOrderingFields(undefined, undefined, undefined)}
            className={classes.appButton}
          />
          <Button text="APPLY" onClick={() => handleSearch()} className={classes.appButton} />
          <Button
            text="CREATE"
            onClick={() => handleModalState(true)}
            className={classes.appButton}
            style={{ marginTop: "5rem" }}
          />
        </div>
      </Card>
    </div>
  );
};

export default memo(MyPlaylists);
