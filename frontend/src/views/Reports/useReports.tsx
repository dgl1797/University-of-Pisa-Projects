import { useCallback, useState } from "react";
import { useSelector } from "react-redux";
import { selectors } from "../../store/reducers";

enum tabs {
  REPORTS = 1,
  PROPOSALS = 2,
}

const useReports = () => {
  const [selectedTab, setSelectedTab] = useState<tabs>(tabs.REPORTS);
  const sessionToken = useSelector(selectors.session.getSessionToken);
  const handleSelection = useCallback((newTab: tabs) => {
    setSelectedTab(newTab);
  }, []);
  return { tabs, selectedTab, sessionToken, handleSelection };
};

export default useReports;
