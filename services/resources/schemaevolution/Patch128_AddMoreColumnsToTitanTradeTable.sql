truncate table TitanTrade

alter table TitanTrade add quotaQuantity [float]
alter table TitanTrade add quotaQuantityUOM [varchar](20)

alter table TitanTrade drop column deliveryDay
alter table TitanTrade add contractDeliveryDay [datetime]

alter table TitanTrade drop column pricingSpec
alter table TitanTrade drop column benchmarkPricingSpec
alter table TitanTrade add contractPricingSpec [varchar](MAX)

alter table TitanTrade drop column deliveryLocation
alter table TitanTrade drop column destinationLocation
alter table TitanTrade add contractDeliveryLocation [varchar](500)
alter table TitanTrade add benchmarkDeliveryLocation [varchar](500) NULL

alter table TitanTrade drop column schedule
alter table TitanTrade drop column expectedSales




