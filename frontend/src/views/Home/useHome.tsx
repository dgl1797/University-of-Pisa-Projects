import { useCallback, useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useMediaQuery } from "react-responsive";
import { useAPIs } from "../../hooks";
import { actions, selectors } from "../../store/reducers";

export default function useHome() {
  const dispatch = useDispatch();
  const statistics = useSelector(selectors.app.getAppInfo);
  const { call } = useAPIs();
  // example of responsive query using react-responsive media query hook
  const isMobile = useMediaQuery({ query: "(max-width: 400px)" });

  const fetchStats = useCallback(() => {
    call("GET", "/infos", {
      callback: (data) => dispatch(actions.app.setAppInfo(data.reply)),
    });
  }, [call, dispatch]);

  useEffect(() => {
    fetchStats();
  }, []);

  useEffect(() => {
    setTimeout(fetchStats, 5000);
  }, [statistics, fetchStats]);

  return {
    statistics,
    isMobile,
  };
}
