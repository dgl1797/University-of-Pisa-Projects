import React, { HTMLAttributes, memo, PropsWithChildren } from "react";
import classes from "./dropmenu.module.css";

type DropMenuProps = {
  options: { key: string; action: () => void }[];
  isOpen?: boolean;
  close: () => void;
  width: number;
  height: number;
} & HTMLAttributes<HTMLDivElement>;

const DropMenu = ({
  options,
  children,
  width,
  height,
  close,
  isOpen = false,
  ...props
}: PropsWithChildren<DropMenuProps>) => {
  return (
    <div style={{ position: "relative" }}>
      {children}
      <div className={isOpen ? classes.openMenu : classes.closedMenu}>
        {options.map((opt) => (
          <div
            key={opt.key}
            className={classes.options}
            onClick={() => {
              opt.action();
              close();
            }}
          >
            {opt.key}
          </div>
        ))}
      </div>
    </div>
  );
};

export default memo(DropMenu);
