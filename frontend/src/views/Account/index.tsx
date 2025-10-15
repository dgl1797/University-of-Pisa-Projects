import React, { memo } from "react";
import classes from "./account.module.css";
import useAccount from "./useAccount";
import popularityIcon from "../../assets/goldenCrown.png";
import { Button, Input } from "../../components";

const Account = () => {
  const {
    userData,
    userInfo,
    edit,
    setEditMode,
    editUsername,
    editedUsername,
    usernameInput,
    handleSave,
    error,
    handleDelete,
  } = useAccount();
  return (
    <div className={classes.page}>
      {error && (
        <div className={classes.errorContainer}>
          <label className={classes.error}>{error}</label>
        </div>
      )}
      {!edit ? (
        <div className={classes.username}>
          <div style={{ cursor: "pointer" }} onClick={() => setEditMode(true)}>
            {editedUsername?.toUpperCase()}
          </div>
        </div>
      ) : (
        <div className={classes.usernameEdit}>
          <Input
            inputType={"text"}
            label={"new username"}
            reference={usernameInput}
            defaultValue={editedUsername}
            onBlur={() => {
              if (editedUsername !== "") setEditMode(false);
            }}
            className={classes.inputContainer}
            focusOnLoad={true}
            onChange={(ev) => editUsername(ev.currentTarget.value)}
          />
        </div>
      )}
      <div className={`${classes.status} ${userInfo?.status === "validator" ? classes.blue : ""}`}>
        {userInfo?.status.toUpperCase() ?? ""}
      </div>
      <div className={`${classes.description} ${classes.separator}`}>
        <div style={{ color: "rgb(0, 11, 172)", fontSize: "1.8rem" }}>INFORMATIONS</div>
        <div>
          number of private playlists:{" "}
          <label style={{ color: "rgb(0, 11, 172)" }}>{userInfo?.numberOfPrivate ?? 0}</label>
        </div>
        <div>
          number of published playlists:{" "}
          <label style={{ color: "rgb(0, 11, 172)" }}>{userInfo?.numberOfPublic ?? 0}</label>
        </div>
      </div>
      <div className={classes.description}>
        <div style={{ color: "rgb(0, 11, 172)", fontSize: "1.8rem" }}>STATISTICS</div>
        <div>
          number of likes given:{" "}
          <label style={{ color: "rgb(0, 11, 172)" }}>{userInfo?.givenLikes ?? 0}</label>
        </div>
        <div>
          your popularity score:{" "}
          <img
            src={popularityIcon}
            alt="score: "
            width={25}
            height={25}
            style={{ marginLeft: "3px", marginRight: "3px" }}
          />
          <label style={{ color: "rgb(0, 11, 172)" }}>{userInfo?.popularityScore ?? 0}</label>
        </div>
        <div>
          your best follower:{" "}
          <label style={{ color: "rgb(0, 11, 172)" }}>
            {userInfo?.bestContributor.username ?? 0}
            {"("}
            <img
              src={popularityIcon}
              alt="score: "
              width={25}
              height={25}
              style={{ marginLeft: "3px", marginRight: "3px" }}
            />
            {userInfo?.bestContributor?.contribution ?? 0}
            {")"}
          </label>
        </div>
      </div>
      <div className={classes.action}>
        {editedUsername !== userData?.username && (
          <Button text="SAVE" style={{ minWidth: "10%" }} onClick={handleSave} />
        )}
        <Button text="DELETE" style={{ minWidth: "10%" }} onClick={handleDelete} />
      </div>
    </div>
  );
};

export default memo(Account);
