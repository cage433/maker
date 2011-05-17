CREATE TABLE dbo.MarketData
	(
	version int NOT NULL IDENTITY (1, 1),
	childVersion int NULL,
	observationDay datetime NOT NULL,
	marketDataSet varchar(128) NOT NULL,
	marketDataType varchar(128) NOT NULL,
	marketDataKey varchar(512) NOT NULL,
	data varchar(MAX) NOT NULL,
	timestamp datetime NOT NULL
	)  ON [PRIMARY]
	 TEXTIMAGE_ON [PRIMARY]

ALTER TABLE dbo.MarketData ADD CONSTRAINT
	PK_MarketData PRIMARY KEY CLUSTERED
	(
	version
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

CREATE NONCLUSTERED INDEX IX_MarketData ON dbo.MarketData
	(
	observationDay,
	marketDataSet,
	marketDataType,
	marketDataKey
	) WITH( STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]

ALTER TABLE dbo.MarketData SET (LOCK_ESCALATION = TABLE)
