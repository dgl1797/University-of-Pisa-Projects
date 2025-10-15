import React, { memo, RefObject } from "react";
import classes from "../pbrowser.module.css";

type FiltersProps = {
  min: RefObject<HTMLInputElement>;
  max: RefObject<HTMLInputElement>;
  year: RefObject<HTMLInputElement>;
  saveAuthor: () => void;
  authorFilter: RefObject<HTMLInputElement>;
  authorDefault: string | undefined;
  title: RefObject<HTMLInputElement>;
  liked: boolean;
  updateSetLiked: (liked: boolean) => void;
};

const Filters = ({
  authorDefault,
  updateSetLiked,
  min,
  max,
  year,
  authorFilter,
  title,
  liked,
  saveAuthor,
}: FiltersProps) => {
  return (
    <div className={classes.filtersContainer}>
      <div className={`${classes.filterElement}`}>
        <label className={classes.filterLabel}>DURATION</label>
        <div>
          <label>min:</label>
          <input className={classes.minMaxInput} type="number" defaultValue={undefined} ref={min} />
          <label>max:</label>
          <input className={classes.minMaxInput} type="number" defaultValue={undefined} ref={max} />
        </div>
      </div>
      <div className={`${classes.filterElement}`}>
        <label className={classes.filterLabel}>AUTHOR</label>
        <input
          type={"text"}
          ref={authorFilter}
          defaultValue={authorDefault}
          onChange={saveAuthor}
        />
      </div>
      <div className={`${classes.filterElement}`}>
        <label className={classes.filterLabel}>TITLE</label>
        <input type={"text"} ref={title} />
      </div>
      <div className={`${classes.filterElement}`}>
        <label className={classes.filterLabel}>YEAR</label>
        <input type={"number"} ref={year} />
      </div>
      <div className={classes.likedButton}>
        <button className={liked ? classes.active : ""} onClick={() => updateSetLiked(!liked)}>
          LIKED
        </button>
      </div>
    </div>
  );
};

export default memo(Filters);
