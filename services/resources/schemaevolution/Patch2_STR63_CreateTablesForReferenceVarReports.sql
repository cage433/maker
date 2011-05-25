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
