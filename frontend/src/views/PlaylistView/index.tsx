import moment from "moment";
import React, { memo } from "react";
import { Button, Input } from "../../components";
import { secondsToDuration } from "../../utils";
import usePlaylistView from "./usePlaylistView";
import classes from "./view.module.css";
import crownIcon from "../../assets/goldenCrown.png";
import SongsModal from "./extra/SongsModal";
import PlaylistsModal from "./extra/PlaylistsModal";
import CloneModal from "./extra/CloneModal";
import { Link } from "react-router-dom";
import { useDispatch } from "react-redux";
import { actions } from "../../store/reducers";

type PlaylistViewProps = {
  isMine?: boolean;
};

const PlaylistView = ({ isMine }: PlaylistViewProps) => {
  const {
    searchBar,
    filteredSongs,
    handleSearch,
    handleActivation,
    handleOrdering,
    orderBy,
    handleRedirect,
    playlistInfo,
    handleSelect,
    selectedSongs,
    selectAll,
    isOpen,
    setIsOpen,
    handleDeletion,
    handleReport,
    successMessage,
    isLoading,
    error,
    deletePlaylist,
    postLike,
    publish,
  } = usePlaylistView(isMine);
  const dispatch = useDispatch();

  return (
    <div className={classes.page}>
      {isMine && !playlistInfo?.isPublic && (
        <div className={classes.publishBtn}>
          <Button text="PUBLISH" style={{ width: "100%" }} onClick={publish} />
        </div>
      )}
      {error && (
        <div className={classes.feedback}>
          <div className={classes.message}>{error}</div>
        </div>
      )}
      {successMessage && (
        <div className={classes.feedback}>
          <div className={classes.smessage}>{successMessage}</div>
        </div>
      )}
      <div className={classes.title}>
        <h1>{playlistInfo?.title ?? ""}</h1>
      </div>
      {playlistInfo?.original ? (
        <div className={classes.subTitle}>
          <h4>
            Cloned from:{" "}
            <Link to={`/dashboard/playlists/${playlistInfo.original._id}`}>
              {playlistInfo.original.title}
            </Link>{" "}
            by:{" "}
            <Link
              to={`/dashboard/playlists`}
              onClick={() =>
                dispatch(
                  actions.playlistFilter.updateAuthorFilter(playlistInfo.original?.author ?? "")
                )
              }
            >
              {playlistInfo.original.author}
            </Link>
          </h4>
        </div>
      ) : (
        <div className={classes.subTitle}> </div>
      )}
      <div className={classes.content}>
        {isMine && <Button text="DELETE" className={classes.deleteBtn} onClick={deletePlaylist} />}
        <div className={classes.infos}>
          <div style={{ fontSize: "1.8rem" }}>{playlistInfo?.author?.toUpperCase()}</div>
          <div className={`${isMine || playlistInfo?.isLiked ? classes.liked : classes.unliked}`}>
            ❤ {playlistInfo?.nLikes}
          </div>
          <div style={{ color: "rgb(0, 11, 172)" }}>
            <img height={23} width={23} src={crownIcon} alt="popularity" />
            {playlistInfo?.popScore}
          </div>
          <div className={classes.searchBar}>
            <Input
              label="song"
              inputType={"text"}
              reference={searchBar}
              className={classes.inputBar}
              onChange={handleSearch}
            />
          </div>
          {!isMine && (
            <Button
              text="👍"
              className={playlistInfo?.isLiked ? classes.likedBtn : ""}
              onClick={postLike}
            />
          )}
          {isMine && <Button text="➕" onClick={() => setIsOpen(true)} disabled={isLoading} />}
          {isMine && selectedSongs.length > 0 && (
            <Button text="🗑️" disabled={isLoading} onClick={handleDeletion} />
          )}
          {!isMine &&
            selectedSongs.length > 0 &&
            selectedSongs.length < (playlistInfo?.songs.length ?? [].length) && (
              <Button text="➕" onClick={() => setIsOpen(true)} disabled={isLoading} />
            )}
          {!isMine &&
            selectedSongs.length > 0 &&
            selectedSongs.length === playlistInfo?.songs.length && (
              <Button text="CLONE" onClick={() => setIsOpen(true)} />
            )}
          {selectedSongs.length > 0 && (
            <Button text="❗" disabled={isLoading} onClick={handleReport} />
          )}
          {isMine && (
            <SongsModal isOpen={isOpen} onClose={() => setIsOpen(false)} onSubmit={() => {}} />
          )}
          {!isMine &&
            selectedSongs.length > 0 &&
            selectedSongs.length < (playlistInfo?.songs.length ?? [].length) && (
              <PlaylistsModal
                isOpen={isOpen}
                onClose={() => setIsOpen(false)}
                onSubmit={() => {}}
                selectedSongs={selectedSongs}
              />
            )}
          {!isMine &&
            selectedSongs.length > 0 &&
            selectedSongs.length === playlistInfo?.songs.length && (
              <CloneModal
                isOpen={isOpen}
                onClose={() => setIsOpen(false)}
                onSubmit={() => {}}
                originalPlaylistId={playlistInfo._id}
              />
            )}
        </div>
        <div className={classes.songList}>
          <div className={classes.songsHeaders}>
            <input
              type={"checkbox"}
              onChange={(ev) => selectAll(ev.target.checked)}
              className={classes.checkbox}
            />
            <div>🎤</div>
            <div
              className={classes.clickable}
              onClick={() => {
                if (orderBy.active === 1) handleOrdering();
                else handleActivation(1);
              }}
            >
              📅
              {`${
                orderBy.active === 1 && orderBy.ordering ? (orderBy.ordering === 1 ? "🡩" : "🡫") : ""
              }`}
            </div>
            <div
              className={classes.clickable}
              onClick={() => {
                if (orderBy.active === 2) handleOrdering();
                else handleActivation(2);
              }}
            >
              🕒
              {`${
                orderBy.active === 2 && orderBy.ordering ? (orderBy.ordering === 1 ? "🡩" : "🡫") : ""
              }`}
            </div>
          </div>
          {filteredSongs?.map((song) => (
            <div className={classes.songCard} key={song.link}>
              <div className={classes.cardClicker} onClick={() => handleRedirect(song.link)} />
              <input
                type={"checkbox"}
                onChange={(ev) =>
                  handleSelect(ev.target.checked, { _id: song._id, link: song.link })
                }
                value={song._id}
                checked={selectedSongs.includes(song._id)}
                className={classes.checkbox}
              />
              <div className={classes.songtitle}>{song.title}</div>
              <div className={classes.songinsertion}>{moment(song.insertionDate).fromNow()}</div>
              <div className={classes.songduration}>{secondsToDuration(song.duration)}</div>
              <div> </div>
              <div className={classes.songartist}>{song.artist}</div>
            </div>
          )) ?? null}
        </div>
      </div>
    </div>
  );
};

export default memo(PlaylistView);
