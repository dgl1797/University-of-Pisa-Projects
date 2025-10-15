import React, { memo, useCallback, useEffect, useState } from "react";
import { Button, Card } from "../../../components";
import { useAPIs } from "../../../hooks";
import { secondsToDuration } from "../../../utils";
import classes from "./extracss.module.css";

type ReplaceModalProps = {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (...args: any) => void;
  songTitle: string;
  songArtist: string;
  songLink: string;
  sessionToken: string;
};

const ReplaceModal = ({
  isOpen,
  onClose,
  onSubmit,
  songTitle,
  songArtist,
  songLink,
  sessionToken,
}: ReplaceModalProps) => {
  const [alternatives, setAlternatives] = useState([]);
  const { call, callOnce } = useAPIs();
  useEffect(() => {
    call("GET", "/songs/search", {
      callback: (data) => setAlternatives(data.reply),
      config: {
        params: {
          songArtist,
          songTitle,
        },
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [call, sessionToken, songArtist, songTitle]);

  const substitute = useCallback(
    (alternative) => {
      callOnce("PUT", "/songs/link", {
        callback: onClose,
        payload: {
          songLink: songLink,
          alternativeData: {
            title: alternative.title,
            link: alternative.link,
            artist: alternative.artist,
            duration: alternative.duration,
          },
        },
        config: {
          headers: {
            Authorization: `Bearer ${sessionToken}`,
          },
        },
      });
    },
    [callOnce, onClose, sessionToken, songLink]
  );

  return (
    <div className={isOpen ? classes.open : classes.closed}>
      <Card title="ALTERNATIVE LINKS" className={classes.modalWindow}>
        <div className={classes.closeButton} onClick={onClose}>
          ✘
        </div>
        <div className={classes.container}>
          {alternatives?.map((alt: any) => (
            <div className={classes.alternativeRow}>
              <div
                className={classes.clicker}
                onClick={() => window.open(alt.link, "_blank")}
              ></div>
              <div className={classes.songDescription}>
                <div className={classes.songTitle}>{alt?.title ?? ""}</div>
                <div className={classes.songArtist}>{alt?.artist ?? ""}</div>
              </div>
              <div className={classes.songDuration}>{secondsToDuration(alt?.duration ?? 0)}</div>
              <div className={classes.vBtn}>
                <Button text="✔️" onClick={() => substitute(alt)} />
              </div>
            </div>
          )) ?? null}
        </div>
      </Card>
    </div>
  );
};

export default memo(ReplaceModal);
