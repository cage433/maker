CREATE TABLE [dbo].[TitanTrade](
	[id] [int] IDENTITY(2947729,1) NOT NULL,
	[tradeID] [char](10) NOT NULL,
	[tradeDay] [datetime] NULL,
	[counterParty] [varchar](255) NULL,
	[instrument] [varchar](50) NULL,
	[quantity] [float] NULL,
	[quantityUOM] [char](20) NULL,
	[commodity] [varchar](255) NULL,
	[deliveryDay] [datetime] NULL,
	[pricingSpec] [varchar](max) NULL,
	[quotaID] [varchar](100) NULL,
	[costs] [varchar](8000) NULL,
	[titanTradeID] [varchar](100) NULL,
	[timestamp] [datetime] NULL,
	[timestampTo_cache] [datetime] NULL,
	[nextVersionId_cache] [int] NULL,
	[expiryDay_cache] [datetime] NULL,
	[error] [varchar](max) NULL
 CONSTRAINT [PK_TitanTrade_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]