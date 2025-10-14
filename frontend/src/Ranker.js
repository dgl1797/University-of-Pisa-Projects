import axios from "axios";
import React, { memo, useCallback, useMemo, useState } from "react";
import style from "./style.module.css";

const mapping = {
  Common: 1,
  Rare: 3,
  Epic: 6,
  Legendary: 8,
  Champion: 10,
};

const levelArray = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14];

const Ranker = (props) => {
  const [selLevels, setSelectedLevels] = useState([1, 1, 1, 1, 1, 1, 1, 1]);
  const selCards = props.selCards;

  const handleSelection = useCallback(
    (index, value) => {
      console.log({
        index,
        value,
      });
      setSelectedLevels(selLevels.map((lvl, i) => (i === index ? value : lvl)));
    },
    [selLevels]
  );

  const renderedSelects = useMemo(() => {
    return selCards.map((sc, index) => (
      <select
        key={sc.CID === -1 ? -1 - index : sc.CID}
        onChange={(ev) => handleSelection(index, ev.target.value)}
      >
        <option selected>Choose</option>
        {levelArray
          .filter((lvl) => lvl >= mapping[`${sc.rarity}`])
          .map((lvl) => (
            <option key={`${sc.CID === -1 ? -1 - index : sc.CID}#${lvl}`} value={lvl}>
              Level: {lvl}
            </option>
          ))}
      </select>
    ));
  }, [selCards, handleSelection]);

  const [calculatedRank, setCalculatedRank] = useState("Calculating...");
  const [hasClicked, setHasClicked] = useState(false);

  const calculateRank = useCallback(() => {
    axios
      .get("http://localhost:5000/api/final_rank", {
        params: {
          c1: JSON.stringify({
            level: selLevels[0],
            CID: selCards[0].CID,
          }),
          c2: JSON.stringify({
            level: selLevels[1],
            CID: selCards[1].CID,
          }),
          c3: JSON.stringify({
            level: selLevels[2],
            CID: selCards[2].CID,
          }),
          c4: JSON.stringify({
            level: selLevels[3],
            CID: selCards[3].CID,
          }),
          c5: JSON.stringify({
            level: selLevels[4],
            CID: selCards[4].CID,
          }),
          c6: JSON.stringify({
            level: selLevels[5],
            CID: selCards[5].CID,
          }),
          c7: JSON.stringify({
            level: selLevels[6],
            CID: selCards[6].CID,
          }),
          c8: JSON.stringify({
            level: selLevels[7],
            CID: selCards[7].CID,
          }),
        },
      })
      .then((response) => {
        setCalculatedRank(response.data);
      })
      .catch((err) => console.log(err));
  }, [selCards, selLevels]);

  return (
    <div className={`${props?.isOpen ? style.modal : style.closedModal}`}>
      <div
        className={style.clicker}
        onClick={() => {
          setCalculatedRank("Calculating...");
          setHasClicked(false);
          if (props?.onClose) props.onClose();
        }}
      ></div>
      <div className={style.rankercontainer}>
        <div className={style.modalTitle}>Select card levels</div>
        {renderedSelects}
      </div>
      {!hasClicked ? (
        <div
          className={`${style.clickable} ${style.modalBtn}`}
          onClick={() => {
            setHasClicked(true);
            calculateRank();
          }}
        >
          CALCULATE
        </div>
      ) : (
        <div
          className={`${style.clickable} ${style.modalBtn}`}
          onClick={() => {
            setHasClicked(false);
            setCalculatedRank("Calculating...");
          }}
        >
          {calculatedRank}
        </div>
      )}
    </div>
  );
};

export default memo(Ranker);
