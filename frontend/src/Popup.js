import React, { memo } from "react";
import style from "./style.module.css";

const Popup = (props) => {
  const infos = props?.infoCard ?? undefined;
  return (
    <div className={`${props?.isOpen ? style.modal : style.closedModal}`}>
      <div
        className={style.clicker}
        onClick={() => {
          if (props?.onClose) props.onClose();
        }}
      ></div>
      <div className={style.container}>
        <div className={style.modalTitle}>
          {infos?.name?.toUpperCase() + ` (x${infos?.count ?? 1})`}
        </div>
        <div className={style.imageContainer}>
          <img src={infos?.imageUrl} alt={infos?.name + " icon"}></img>
        </div>
        <div className={style.infos}>
          <div>
            Cost: <div className={style.attrib}>{infos?.elixir}</div>
          </div>
          <div>
            Rarity: <div className={style.attrib}>{infos?.rarity}</div>
          </div>
          <div>
            Type: <div className={style.attrib}>{infos?.type}</div>
          </div>
          {infos?.hitpoints && (
            <div>
              HP/LVL: <div className={style.attrib}>{JSON.stringify(infos.hitpoints)}</div>
            </div>
          )}
          {infos?.damage && (
            <div>
              DMG/LVL: <div className={style.attrib}>{JSON.stringify(infos.damage)}</div>
            </div>
          )}
          {infos?.duration && (
            <div>
              Duration: <div className={style.attrib}>{infos.duration + "s"}</div>
            </div>
          )}
          {infos?.hps && (
            <div>
              ATK Speed: <div className={style.attrib}>{infos.hps + "s"}</div>
            </div>
          )}
          {infos?.hitspeed && (
            <div>
              Action Frequency: <div className={style.attrib}>{infos.hitspeed + "s"}</div>
            </div>
          )}
          {infos?.range !== undefined && (
            <div>
              Range: <div className={style.attrib}>{infos.range}</div>
            </div>
          )}
          {infos?.radius !== undefined && (
            <div>
              Radius: <div className={style.attrib}>{infos.radius}</div>
            </div>
          )}
          {infos?.speed && (
            <div>
              MOV Speed: <div className={style.attrib}>{infos.speed}</div>
            </div>
          )}
          {infos?.attacks_air && <div className={style.attrib}>Hits air units</div>}
          {infos?.hits_air && <div className={style.attrib}>Affects air units</div>}
          {infos?.attacks_ground && <div className={style.attrib}>Hits ground units</div>}
          {infos?.hits_ground && <div className={style.attrib}>Affects ground units</div>}
          {infos?.target_only_buildings && (
            <div className={style.attrib}>Targets only buildings</div>
          )}
          {infos?.ignore_buildings === false && (
            <div className={style.attrib}>Affects buildings</div>
          )}
          {infos?.target_only_troops && <div className={style.attrib}>Targets only troops</div>}
          {infos?.only_allies && <div className={style.attrib}>Doesn't affect enemies</div>}
          {infos?.only_enemies && <div className={style.attrib}>Doesn't affect allies</div>}
          {infos?.ability && <div className={style.attrib}>has ability</div>}
        </div>
      </div>
    </div>
  );
};

export default memo(Popup);
