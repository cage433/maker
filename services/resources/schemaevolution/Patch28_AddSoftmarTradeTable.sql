CREATE TABLE [dbo].[SoftmarTrade](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[tradeID] [char](10) NOT NULL,
	[tradeDay] [datetime] NULL,
	[counterParty] [varchar](255) NULL,
	[instrument] [varchar](50) NULL,
	[volume] [float] NULL,
	[volumeUOM] [char](20) NULL,
	[strike] [float] NULL,
	[strikeUOM] [char](20) NULL,
	[market] [varchar](255) NULL,
	[startDate] [datetime] NULL,
	[endDate] [datetime] NULL,
	[timestamp] [datetime] NULL,
	[error] [varchar](max) NULL,
	[enteredBy] [varchar](100) NULL,
	[portfolio] [varchar](255) NULL,
	[systemTimestamp] [datetime] NULL,
	[timestampTo_cache] [datetime] NULL,
	[nextVersionId_cache] [int] NULL,
	[expiryDay_cache] [datetime] NULL,
	[firstSpreadPeriod] [varchar](255) NULL,
	[secondSpreadPeriod] [varchar](255) NULL,
 CONSTRAINT [PK_SoftmarTrade_id] PRIMARY KEY NONCLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

CREATE TABLE [dbo].[SoftmarTradeUTP](
	[id] [int] NULL,
	[instrumentid] [int] NULL,
	[utpVolume] [float] NULL
) ON [PRIMARY]

