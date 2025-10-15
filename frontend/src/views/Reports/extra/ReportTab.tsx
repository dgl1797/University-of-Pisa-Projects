import React, { memo, useCallback, useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { Button } from "../../../components";
import { useAPIs } from "../../../hooks";
import { actions, selectors } from "../../../store/reducers";
import classes from "./extracss.module.css";
import ReplaceModal from "./ReplaceModal";

type ReportTabProps = {
  session: string;
};

const ReportTab = ({ session }: ReportTabProps) => {
  const { call, callOnce } = useAPIs();
  const dispatch = useDispatch();
  const reportedSongs = useSelector(selectors.songList.getReported);
  const [ordering, setOrdering] = useState<-1 | 1 | undefined>(undefined);
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const [acceptedSong, setAcceptedSong] = useState(null);

  const handleReports = useCallback(
    (data) => {
      dispatch(actions.songList.setReports(data.reply));
    },
    [dispatch]
  );
  const handleOrdering = useCallback(() => {
    if (ordering === undefined) setOrdering(1);
    if (ordering === 1) setOrdering(-1);
    if (ordering === -1) setOrdering(undefined);
  }, [ordering]);
  useEffect(() => {
    call("GET", "/songs/reports", {
      callback: handleReports,
      config: {
        params: {
          orderByReps: ordering,
          isReport: true,
        },
        headers: {
          Authorization: `Bearer ${session}`,
        },
      },
    });
  }, [call, handleReports, session, ordering]);
  const removeReport = useCallback(
    (link: string) => {
      callOnce("DELETE", `/songs/reports`, {
        callback: (data) => dispatch(actions.songList.deleteReportProposal(link)),
        config: {
          params: {
            link: link,
          },
          headers: {
            Authorization: `Bearer ${session}`,
          },
        },
      });
    },
    [callOnce, session, dispatch]
  );
  const handleModalOpen = useCallback((acceptedSong) => {
    setAcceptedSong(acceptedSong);
    setIsOpen(true);
  }, []);
  return (
    <div className={classes.container}>
      {isOpen && (
        <ReplaceModal
          isOpen={isOpen}
          onClose={() => setIsOpen(false)}
          onSubmit={() => {}}
          sessionToken={session}
          songArtist={(acceptedSong as any)?.artist ?? ""}
          songTitle={(acceptedSong as any)?.title ?? ""}
          songLink={(acceptedSong as any)?.link ?? ""}
        />
      )}
      <div className={classes.headers}>
        <div onClick={handleOrdering} style={{ cursor: "pointer" }}>
          ❗ {ordering !== undefined ? (ordering === 1 ? "🡩" : "🡫") : ""}
        </div>
        <div>SONGS</div>
        <div>ACCEPT</div>
        <div>DECLINE</div>
      </div>
      {reportedSongs?.map((ps) => (
        <div className={classes.songRow} key={ps.link}>
          <div>{ps.nReps}</div>
          <div className={classes.song} onClick={() => window.open(ps.link, "_blank")}>
            <div className={classes.songTitle}>{ps.title}</div>
            <div className={classes.songArtist}>{ps.artist}</div>
          </div>
          <div>
            <Button text="✔️" onClick={() => handleModalOpen(ps)} />
          </div>
          <div>
            <Button text="🗑️" onClick={() => removeReport(ps.link)} />
          </div>
        </div>
      ))}
    </div>
  );
};

export default memo(ReportTab);
