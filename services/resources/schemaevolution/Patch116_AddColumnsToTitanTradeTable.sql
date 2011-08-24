truncate table TitanTrade

alter table TitanTrade add comment [varchar](255) NULL
alter table TitanTrade add submitted [datetime]  NULL
alter table TitanTrade add shape [varchar](100) NULL
alter table TitanTrade add grade [varchar](100) NULL
alter table TitanTrade add deliveryLocation [varchar](500) NULL
alter table TitanTrade add destinationLocation [varchar](500) NULL

alter table TitanTrade add contractFinalised [varchar](10) NULL
alter table TitanTrade add tolerancePlus [float] NULL
alter table TitanTrade add toleranceMinus [float] NULL
alter table TitanTrade add schedule [datetime] NULL
alter table TitanTrade add expectedSales [datetime] NULL

