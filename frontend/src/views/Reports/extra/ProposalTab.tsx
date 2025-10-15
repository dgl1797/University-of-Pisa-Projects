import React, { memo, useCallback, useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { Button } from "../../../components";
import { useAPIs } from "../../../hooks";
import { actions, selectors } from "../../../store/reducers";
import classes from "./extracss.module.css";

type ProposalTabProps = {
  session: string;
};

const ProposalTab = ({ session }: ProposalTabProps) => {
  const { call, callOnce } = useAPIs();
  const dispatch = useDispatch();
  const proposedSongs = useSelector(selectors.songList.getProposals);
  const [ordering, setOrdering] = useState<-1 | 1 | undefined>(undefined);

  const handleProposals = useCallback(
    (data) => {
      dispatch(actions.songList.setProposals(data.reply));
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
      callback: handleProposals,
      config: {
        params: {
          orderByReps: ordering,
          isReport: false,
        },
        headers: {
          Authorization: `Bearer ${session}`,
        },
      },
    });
  }, [call, handleProposals, session, ordering]);

  const confirmAddition = useCallback(
    (link: string) => {
      callOnce("POST", `/songs`, {
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
    [dispatch, session, callOnce]
  );
  const removeAddition = useCallback(
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
    [callOnce, dispatch, session]
  );

  return (
    <div className={classes.container}>
      <div className={classes.headers}>
        <div onClick={handleOrdering} style={{ cursor: "pointer" }}>
          ❗ {ordering !== undefined ? (ordering === 1 ? "🡩" : "🡫") : ""}
        </div>
        <div>SONGS</div>
        <div>ACCEPT</div>
        <div>DECLINE</div>
      </div>
      {proposedSongs?.map((ps) => (
        <div className={classes.songRow} key={ps.link}>
          <div>{ps.nReps}</div>
          <div className={classes.song} onClick={() => window.open(ps.link, "_blank")}>
            <div className={classes.songTitle}>{ps.title}</div>
            <div className={classes.songArtist}>{ps.artist}</div>
          </div>
          <div>
            <Button text="✔️" onClick={() => confirmAddition(ps.link)} />
          </div>
          <div>
            <Button text="🗑️" onClick={() => removeAddition(ps.link)} />
          </div>
        </div>
      ))}
    </div>
  );
};

export default memo(ProposalTab);
