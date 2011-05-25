
CREATE TABLE [dbo].[EAITrade](
	[id] [int] IDENTITY(2947729,1) NOT NULL,
	[tradeID] [int],
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
	[averagingPeriodStart] [datetime] NULL,
	[averagingPeriodEnd] [datetime] NULL,
	[exerciseDay] [datetime] NULL,
	[maturityDay] [datetime] NULL,
	[callPut] [char](1) NULL,
	[timestamp] [datetime] NULL,
	[error] [varchar](max) NULL,

	[bookID] [int],
	[dealID] [int],
	[strategyID] [int]
 CONSTRAINT [PK_EAITrade_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
