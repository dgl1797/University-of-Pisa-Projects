import React, { HTMLAttributes, memo, PropsWithChildren } from "react";
import classes from "./tooltip.module.css";
import useTooltip from "./useTooltip";

type TooltipProps = {
  text: string;
} & HTMLAttributes<HTMLDivElement>;

const Tooltip = ({ text, children, ...props }: PropsWithChildren<TooltipProps>) => {
  const { showTooltip, hideTooltip, tooltipRef } = useTooltip();
  return (
    <div className={classes.container} onMouseOver={showTooltip} onMouseOut={hideTooltip}>
      {children}
      <div ref={tooltipRef} className={classes.tooltip} style={{ visibility: "hidden" }}>
        playlists
      </div>
    </div>
  );
};

export default memo(Tooltip);
