import { useCallback, useRef } from "react";

const useTooltip = () => {
  const tooltipRef = useRef<HTMLDivElement>(null);

  const showTooltip = useCallback(() => {
    if (tooltipRef.current) tooltipRef.current.style.visibility = "visible";
  }, []);

  const hideTooltip = useCallback(() => {
    if (tooltipRef.current) tooltipRef.current.style.visibility = "hidden";
  }, []);

  return { tooltipRef, showTooltip, hideTooltip };
};

export default useTooltip;
