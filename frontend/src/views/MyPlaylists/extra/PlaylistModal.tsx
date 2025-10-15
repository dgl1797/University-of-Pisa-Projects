import React, { memo, useCallback, useRef } from "react";
import { Button, Card, Input } from "../../../components";
import classes from "./extracss.module.css";

type PlaylistModalProps = {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (...args: any) => void;
};

const PlaylistModal = ({ isOpen, onClose, onSubmit }: PlaylistModalProps) => {
  const titleRef = useRef<HTMLInputElement>(null);
  const validateTitle = useCallback((value: string) => {
    return value.length > 0;
  }, []);
  return (
    <div className={`${isOpen ? classes.open : classes.closed}`}>
      <div className={classes.backgroundClicker} onClick={onClose}></div>
      <Card title="NEW PLAYLIST" className={classes.modalWindow}>
        <Input
          inputType={"text"}
          reference={titleRef}
          label="TITLE"
          hasError={[
            {
              valid: validateTitle,
              errorMessage: "field can not be empty",
            },
          ]}
          className={classes.modalInput}
        />
        <Button text="➕ ADD" onClick={() => onSubmit(titleRef.current?.value ?? "")} />
      </Card>
    </div>
  );
};

export default memo(PlaylistModal);
