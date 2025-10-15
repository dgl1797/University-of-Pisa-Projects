import React, { memo } from "react";
import useReports from "./useReports";
import classes from "./reports.module.css";
import { Card } from "../../components";
import ReportTab from "./extra/ReportTab";
import ProposalTab from "./extra/ProposalTab";

const Reports = () => {
  const { tabs, selectedTab, sessionToken, handleSelection } = useReports();
  return (
    <div className={classes.page}>
      <Card className={classes.card}>
        <div className={classes.tabs}>
          <div
            className={selectedTab === tabs.REPORTS ? classes.active : classes.inactive}
            onClick={() => handleSelection(tabs.REPORTS)}
          >
            REPORTS
          </div>
          <div
            className={selectedTab === tabs.PROPOSALS ? classes.active : classes.inactive}
            onClick={() => handleSelection(tabs.PROPOSALS)}
          >
            PROPOSALS
          </div>
        </div>
        {selectedTab === tabs.REPORTS && <ReportTab session={sessionToken ?? ""} />}
        {selectedTab === tabs.PROPOSALS && <ProposalTab session={sessionToken ?? ""} />}
      </Card>
    </div>
  );
};

export default memo(Reports);
