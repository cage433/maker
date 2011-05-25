ALTER TABLE VarReportHeader MODIFY snapshotId int(11) NULL;

ALTER TABLE VarReport MODIFY commodity           varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY desk                varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY groupCompany        varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY location            varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY portfolio           varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY strategy            varchar(50) NOT NULL;
ALTER TABLE VarReport MODIFY trader              varchar(50) NOT NULL;

ALTER TABLE VarReport MODIFY VaR95               double NULL;
ALTER TABLE VarReport MODIFY VaR95UOM            varchar(50) NULL;
ALTER TABLE VarReport MODIFY CVaR95              double NULL;
ALTER TABLE VarReport MODIFY CVaR95UOM           varchar(50) NULL;
ALTER TABLE VarReport MODIFY VaR99               double NULL;
ALTER TABLE VarReport MODIFY VaR99UOM            varchar(50) NULL;
ALTER TABLE VarReport MODIFY CVaR99              double NULL;
ALTER TABLE VarReport MODIFY CVaR99UOM           varchar(50) NULL;
ALTER TABLE VarReport MODIFY standardErrors95    double NULL;
ALTER TABLE VarReport MODIFY standardErrors95UOM varchar(50) NULL;
ALTER TABLE VarReport MODIFY netPosition         double NULL;
ALTER TABLE VarReport MODIFY netPositionUOM      varchar(50) NULL;

ALTER TABLE VarReport ADD UNIQUE(varReportHeaderId, commodity, desk, groupCompany, location, portfolio, strategy, trader);