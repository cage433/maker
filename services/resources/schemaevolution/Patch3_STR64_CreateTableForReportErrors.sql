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