truncate table TitanTrade

alter table TitanTrade add benchmarkPricingSpec [varchar](max) NULL
alter table TitanTrade add benchmarkDeliveryDay [datetime]  NULL
alter table TitanTrade add isPurchase [varchar](5)
alter table TitanTrade add inventoryQuantity [float]
alter table TitanTrade add inventoryQuantityUOM [varchar](20) 

