import React, { memo, useCallback, useRef } from "react";
import { useSelector } from "react-redux";
import { useNavigate } from "react-router-dom";
import { Button, Card, Input } from "../../../components";
import { useAPIs } from "../../../hooks";
import { selectors } from "../../../store/reducers";
import classes from "./extracss.module.css";

type CloneModalProps = {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (...args: any) => void;
  originalPlaylistId: string;
};

const CloneModal = ({ isOpen, onClose, onSubmit, originalPlaylistId }: CloneModalProps) => {
  const titleRef = useRef<HTMLInputElement>(null);
  const navigate = useNavigate();
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const userData = useSelector(selectors.user.getUserData);
  const { isLoading, callOnce } = useAPIs();

  const handleCretionSucess = useCallback(
    (data) => {
      onClose();
      navigate(`/dashboard/${userData?._id}/playlists/${data.reply}`);
    },
    [navigate, userData, onClose]
  );

  const handleCall = useCallback(() => {
    callOnce("POST", `/playlists/${originalPlaylistId}/clone`, {
      callback: handleCretionSucess,
      payload: { title: titleRef.current?.value ?? "" },
      config: {
        headers: {
          Authorization: `Bearer ${sessionToken}`,
        },
      },
    });
  }, [callOnce, handleCretionSucess, originalPlaylistId, sessionToken]);
  return (
    <div className={isOpen ? classes.open : classes.closed}>
      <Card title="NEW PLAYLIST" className={classes.modalWindow}>
        <div className={classes.closeButton} onClick={onClose}>
          ✘
        </div>
        <Input
          inputType={"text"}
          label="TITLE"
          reference={titleRef}
          className={`${classes.modalInput} ${classes.lowerLabel}`}
        />
        <Button
          text="CONFIRM"
          disabled={isLoading}
          onClick={handleCall}
          className={classes.confirmBtn}
        />
      </Card>
    </div>
  );
};

export default memo(CloneModal);
