truncate table TitanTrade

alter table TitanTrade add comment [varchar](255) NULL
alter table TitanTrade add submitted [datetime]  NULL
alter table TitanTrade add shape [varchar](50) NULL
alter table TitanTrade add grade [varchar](50) NULL
alter table TitanTrade add deliveryLocation [varchar](500) NULL
alter table TitanTrade add destinationLocation [varchar](500) NULL

