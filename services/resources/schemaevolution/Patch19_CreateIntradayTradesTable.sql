
CREATE TABLE [dbo].[IntradayTrades](
	[id] [int] IDENTITY(2947729,1) NOT NULL,
	[tradeID] [char](10) NOT NULL,
	[tradeDay] [datetime] NULL,
	[counterParty] [varchar](255) NULL,
	[instrument] [varchar](50) NULL,
	[volume] [float] NULL,
	[volumeUOM] [char](20) NULL,
	[strike] [float] NULL,
	[strikeUOM] [char](20) NULL,
	[spread] [float] NULL,
	[spreadUOM] [char](20) NULL,
	[market] [varchar](255) NULL,
	[fixingIndex] [varchar](50) NULL,
	[lastTradingDay] [datetime] NULL,
	[deliveryDay] [datetime] NULL,
	[startDate] [datetime] NULL,
	[endDate] [datetime] NULL,
	[exerciseDay] [datetime] NULL,
	[maturityDay] [datetime] NULL,
	[callPut] [char](1) NULL,
	[timestamp] [datetime] NULL,
	[error] [varchar](max) NULL,
	[bookID] [int] NULL,
	[dealID] [int] NULL,
	[strategyID] [int] NULL,
	[timestampTo_cache] [datetime] NULL,
	[nextVersionId_cache] [int] NULL,
	[expiryDay_cache] [datetime] NULL,
	[delivery] [char](20) NULL,
	[fixedRate] [float] NULL,
	[fixedBasis] [varchar](50) NULL,
	[floatBasis] [varchar](50) NULL,
	[fixedPaymentFreq] [varchar](50) NULL,
	[floatPaymentFreq] [varchar](50) NULL,
	[optionType] [nvarchar](15) NULL,
 CONSTRAINT [PK_IntradayTrades_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

CREATE TABLE [dbo].[IntradayTradesUTP](
	[id] [int] NULL,
	[instrumentid] [int] NULL,
	[utpVolume] [float] NULL
) ON [PRIMARY]
