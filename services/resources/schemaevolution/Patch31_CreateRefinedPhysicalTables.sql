CREATE TABLE [dbo].[RefinedAssignment](
	[id] [int] IDENTITY(1,1) NOT NULL,

	[tradeID] [char](50) NOT NULL,
	[counterParty] [varchar](255) NULL,
	[tradeDay] [datetime] NULL,

	[estimatedDelivery] [datetime] NULL,
	[market] [varchar](255) NULL,
	[volume] [float] NULL,
	[volumeUOM] [char](20) NULL,

	[groupCompany] [varchar](255) NULL,
	[exchange] [varchar](10) NULL,
	[hub] [varchar](100) NULL,
	[commodityCategory] [varchar](100) NULL,
	[contractNo] [varchar](100) NULL,
	[allocationNo] [varchar](100) NULL,

	[timestamp] [datetime] NULL,
	[error] [varchar](max) NULL,
	[systemTimestamp] [datetime] NULL,
	[timestampTo_cache] [datetime] NULL,
	[nextVersionId_cache] [int] NULL,
	[expiryDay_cache] [datetime] NULL,
	[instrument] [varchar](50) NULL,
	

	
 CONSTRAINT [PK_RefinedAssignment_id] PRIMARY KEY NONCLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

CREATE TABLE [dbo].[RefinedAssignmentUTP](
	[id] [int] NULL,
	[instrumentid] [int] NULL,
	[utpVolume] [float] NULL
) ON [PRIMARY]

alter table Instrument add
 estimatedDelivery datetime



CREATE TABLE [dbo].[RefinedFixation](
	[id] [int] IDENTITY(1,1) NOT NULL,

	[tradeID] [char](50) NOT NULL,
	[counterParty] [varchar](255) NULL,
	[tradeDay] [datetime] NULL,

	[fixations] [varchar](max) NULL,


	[groupCompany] [varchar](255) NULL,
	[exchange] [varchar](10) NULL,
	[riskExchange] [varchar](10) NULL,
	[contractNo] [varchar](100) NULL,
	[pricingType] [varchar](100) NULL,

	[timestamp] [datetime] NULL,
	[error] [varchar](max) NULL,
	[systemTimestamp] [datetime] NULL,
	[timestampTo_cache] [datetime] NULL,
	[nextVersionId_cache] [int] NULL,
	[expiryDay_cache] [datetime] NULL,
	[instrument] [varchar](50) NULL,



 CONSTRAINT [PK_RefinedFixation_id] PRIMARY KEY NONCLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

CREATE TABLE [dbo].[RefinedFixationUTP](
	[id] [int] NULL,
	[instrumentid] [int] NULL,
	[utpVolume] [float] NULL
) ON [PRIMARY]



alter table Instrument add
 fixationDate datetime

alter table Instrument add
 isAverageFixation char(1)
