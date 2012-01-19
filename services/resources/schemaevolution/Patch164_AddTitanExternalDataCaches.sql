CREATE TABLE [dbo].[EDMQuotaDetails](
 [quotaID] [varchar](50) NOT NULL,
 [details] [varchar](MAX) NOT NULL
)

CREATE TABLE [dbo].[EDMLogisticsInventory](
 [inventoryID] [varchar](50) NOT NULL,
 [inventory]   [varchar](MAX) NOT NULL
)

CREATE TABLE [dbo].[EDMQuotaFullyAllocated](
 [quotaID] [varchar](50) NOT NULL,
 [isAllocated] [bit] NOT NULL
)
