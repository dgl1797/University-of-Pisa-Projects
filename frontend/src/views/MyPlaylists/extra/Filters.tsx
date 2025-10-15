import React, { memo, RefObject } from "react";
import classes from "../myplaylists.module.css";

type FiltersProps = {
  min: RefObject<HTMLInputElement>;
  max: RefObject<HTMLInputElement>;
  year: RefObject<HTMLInputElement>;
  title: RefObject<HTMLInputElement>;
  isPublic?: boolean;
  updatePublic: () => void;
};

const Filters = ({ isPublic, updatePublic, min, max, year, title }: FiltersProps) => {
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
        <label className={classes.filterLabel}>TITLE</label>
        <input type={"text"} ref={title} />
      </div>
      <div className={`${classes.filterElement}`}>
        <label className={classes.filterLabel}>YEAR</label>
        <input type={"number"} ref={year} />
      </div>
      <div className={classes.likedButton}>
        <button
          className={`${isPublic !== undefined ? classes.active : ""}`}
          onClick={updatePublic}
        >
          PUBLIC {`${isPublic === true ? "(Pu)" : isPublic === false ? "(Pr)" : ""}`}
        </button>
      </div>
    </div>
  );
};

export default memo(Filters);
