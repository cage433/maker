-- --------------------------------------------------------

--
-- Drop the tables if they exist already
--
DROP TABLE IF EXISTS `SnapshotData`;
DROP TABLE IF EXISTS `Snapshot`;
DROP TABLE IF EXISTS `Portfolio`;
DROP TABLE IF EXISTS `Trade`;
DROP TABLE IF EXISTS `Portfolio`;
DROP TABLE IF EXISTS `VarReportError`;
DROP TABLE IF EXISTS `VarReport`;
DROP TABLE IF EXISTS `VarReportHeader`;
DROP TABLE IF EXISTS `ReferenceVarReportValue`;
DROP TABLE IF EXISTS `ReferenceVarReportColumn`;
DROP TABLE IF EXISTS `ReferenceVarReportRow`;
DROP TABLE IF EXISTS `ReferenceVarReportHeader`;
DROP TABLE IF EXISTS `Indexes`;


-- --------------------------------------------------------

--
-- Table structure for table `Portfolio`
--


CREATE TABLE IF NOT EXISTS `Portfolio` (
  `portfolio` varchar(50) NOT NULL DEFAULT '',
  `location` varchar(50) DEFAULT NULL,
  `strategy` varchar(50) DEFAULT NULL,
  `desk` varchar(50) DEFAULT NULL,
  `commodity` varchar(50) DEFAULT NULL,
  `groupCompany` varchar(50) DEFAULT NULL,
  `trader` varchar(50) DEFAULT NULL,
  `reportingEntity` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`portfolio`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

-- --------------------------------------------------------

--
-- Table structure for table `Snapshot`
--


CREATE TABLE IF NOT EXISTS `Snapshot` (
  `snapshotID` int(11) NOT NULL AUTO_INCREMENT,
  `revalGroup` varchar(40) NOT NULL,
  `observationDay` date NOT NULL,
  `snapshotTime` datetime NOT NULL,
  `trinitySnapshotID` int(11) DEFAULT NULL,
  PRIMARY KEY (`snapshotID`,`revalGroup`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=1668 ;

-- --------------------------------------------------------

--
-- Table structure for table `SnapshotData`
--


CREATE TABLE IF NOT EXISTS `SnapshotData` (
  `snapshotID` int(11) NOT NULL,
  `dataTypeKey` varchar(255) NOT NULL,
  `subTypeKey` varchar(255) NOT NULL,
  `data` longtext NOT NULL,
  PRIMARY KEY (`snapshotID`,`dataTypeKey`,`subTypeKey`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='SnapshotData';

-- --------------------------------------------------------

--
-- Table structure for table `Trade`
--


CREATE TABLE IF NOT EXISTS `Trade` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `systemName` varchar(50) DEFAULT NULL,
  `tradeID` int(11) DEFAULT NULL,
  `tradeDay` date DEFAULT NULL,
  `scratch` tinyint,
  `enteredBy` varchar(100) DEFAULT NULL,
  `counterParty` varchar(255) DEFAULT NULL,
  `portfolio` varchar(255) DEFAULT NULL,
  `instrument` varchar(50) DEFAULT NULL,
  `volume` double DEFAULT NULL,
  `volumeUOM` char(20) DEFAULT NULL,
  `strike` double DEFAULT NULL,
  `strikeUOM` char(20) DEFAULT NULL,
  `spread` double DEFAULT NULL,
  `spreadUOM` char(20) DEFAULT NULL,
  `market` varchar(255) DEFAULT NULL,
  `fixingIndex` varchar(50) DEFAULT NULL,
  `lastTradingDay` date DEFAULT NULL,
  `deliveryDay` date DEFAULT NULL,
  `averagingPeriodStart` date DEFAULT NULL,
  `averagingPeriodEnd` date DEFAULT NULL,
  `exerciseDay` date DEFAULT NULL,
  `maturityDay` date DEFAULT NULL,
  `callPut` char(1) DEFAULT NULL,
  `systemTimestamp` datetime DEFAULT NULL,
  `timestamp` datetime DEFAULT NULL,
  `state` char(1) DEFAULT NULL,
  `error` text,
  PRIMARY KEY (`id`),
  KEY `trinityInstance` (`systemName`),
  KEY `tradeID` (`tradeID`),
  KEY `id` (`id`,`systemName`),
  KEY `id_2` (`id`,`tradeID`)
) ENGINE=InnoDB  DEFAULT CHARSET=latin1 AUTO_INCREMENT=358337 ;


-- --------------------------------------------------------

--
-- Table structure for table `VarReport`
--

CREATE TABLE IF NOT EXISTS `Portfolio` (
  `portfolio` varchar(50) NOT NULL DEFAULT '',
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `location` varchar(50) DEFAULT NULL,
  `strategy` varchar(50) DEFAULT NULL,
  `desk` varchar(50) DEFAULT NULL,
  `commodity` varchar(50) DEFAULT NULL,
  `groupCompany` varchar(50) DEFAULT NULL,
  `trader` varchar(50) DEFAULT NULL,
  `reportingEntity` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`portfolio`),
  KEY `id` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



-- --------------------------------------------------------

--
-- Table structure for table `VarReportHeader`
--

CREATE TABLE IF NOT EXISTS `VarReportHeader` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `snapshotId` int(11) NULL,
  `observationDay` date NOT NULL,
  `reportingEntityName` varchar(50) NOT NULL,
  `generationDateAndTime` dateTime NOT NULL,
  `timeTakenToGenerate` int unsigned NOT NULL,
  `reportEmailed` bool NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE(snapshotId, observationDay, reportingEntityName)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


-- --------------------------------------------------------

--
-- Table structure for table `VarReport`
--

CREATE TABLE IF NOT EXISTS `VarReport` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `varReportHeaderId` int(11) NOT NULL,
  `commodity` varchar(50) NOT NULL,
  `desk` varchar(50) NOT NULL,
  `groupCompany` varchar(50) NOT NULL,
  `location` varchar(50) NOT NULL,
  `portfolio` varchar(50) NOT NULL,
  `strategy` varchar(50) NOT NULL,
  `trader` varchar(50) NOT NULL,
  `VaR95` double NULL,
  `VaR95UOM` varchar(50) NULL,
  `CVaR95` double NULL,
  `CVaR95UOM` varchar(50) NULL,
  `VaR99` double NULL,
  `VaR99UOM` varchar(50) NULL,
  `CVaR99` double NULL,
  `CVaR99UOM` varchar(50) NULL,
  `standardErrors95` double NULL,
  `standardErrors95UOM` varchar(50) NULL,
  `netPosition` double NULL,
  `netPositionUOM` varchar(50) NULL,
  PRIMARY KEY (`id`),
  INDEX (varReportHeaderId),
  UNIQUE(varReportHeaderId, commodity, desk, groupCompany, location, portfolio, strategy, trader),
  FOREIGN KEY (varReportHeaderId) REFERENCES VarReportHeader(id)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



-- --------------------------------------------------------

--
-- Table structure for table `VarReportError`
--

CREATE TABLE IF NOT EXISTS `VarReportError` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `varReportId` int(11) NULL,
  `instrument` TEXT,
  `error` TEXT,
  PRIMARY KEY (`id`),
  FOREIGN KEY (varReportId) REFERENCES VarReport(id)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



-- --------------------------------------------------------

--
-- Table structure for table `ReferenceVarReportHeader`
--

CREATE TABLE IF NOT EXISTS `ReferenceVarReportHeader` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `snapshotId` int(11) NULL,
  `observationDay` date NOT NULL,
  `reportingEntityName` varchar(50) NOT NULL,
  `generationDateAndTime` dateTime NOT NULL,
  `timeTakenToGenerate` int unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE(snapshotId, observationDay, reportingEntityName)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


-- --------------------------------------------------------

--
-- Table structure for table `ReferenceVarReportColumn`
--

CREATE TABLE IF NOT EXISTS `ReferenceVarReportColumn` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `referenceVarReportHeaderId` int(11) NOT NULL,
  `month` date NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (referenceVarReportHeaderId) REFERENCES ReferenceVarReportHeader(id),
  UNIQUE(referenceVarReportHeaderId, month)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


-- --------------------------------------------------------

--
-- Table structure for table `ReferenceVarReportRow`
--

CREATE TABLE IF NOT EXISTS `ReferenceVarReportRow` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `referenceVarReportHeaderId` int(11) NOT NULL,
  `market` varchar(50) NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (referenceVarReportHeaderId) REFERENCES ReferenceVarReportHeader(id),
  UNIQUE(referenceVarReportHeaderId, market)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



-- --------------------------------------------------------

--
-- Table structure for table `ReferenceVarReportValue`
--

CREATE TABLE IF NOT EXISTS `ReferenceVarReportValue` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `referenceVarReportRowId` int(11) NOT NULL,
  `referenceVarReportColumnId` int(11) NOT NULL,
  `referenceValue` double NOT NULL,
  `referenceValueUOM` varchar(50) NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (referenceVarReportRowId) REFERENCES ReferenceVarReportRow(id),
  FOREIGN KEY (referenceVarReportColumnId) REFERENCES ReferenceVarReportColumn(id)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;



-- --------------------------------------------------------

--
-- Table structure for table `Indexes`
--

CREATE TABLE IF NOT EXISTS `Indexes` (
  `name` varchar(255) NOT NULL,
  `marketName` varchar(255) NOT NULL,
  `uom` char(20) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
