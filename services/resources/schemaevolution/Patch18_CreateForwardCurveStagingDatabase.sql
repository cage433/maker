CREATE TABLE [dbo].[ForwardCurveStaging](
	[observationDate] [datetime] NOT NULL,
	[timestamp] [datetime] NOT NULL,
	[curveID] [int] NOT NULL,
	[pricingGroupID] [int] NOT NULL,
	[subgroupName] [varchar](255) NULL,
	[version] [int] NOT NULL,
	[childVersion] [int] NULL,
	[data] [varchar](max) NOT NULL,
 CONSTRAINT [IX_ForwardCurveStaging] UNIQUE NONCLUSTERED (
	[observationDate] ASC,
	[pricingGroupID] ASC,
	[subgroupName] ASC,
	[curveID] ASC,
	[version] ASC
))
