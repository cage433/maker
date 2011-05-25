-- --------------------------------------------------------

--
-- Table structure for table `SchemaEvolutionPatch`
--

CREATE TABLE `SchemaEvolutionPatch` (
  `id`               int(11)      NOT NULL AUTO_INCREMENT,
  `patchNumber`      int(11)      NOT NULL,
  `patchName`        varchar(50)  NOT NULL,
  `patchDescription` varchar(255) NOT NULL,
  `dateApplied`      datetime     NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
