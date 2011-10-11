truncate table TitanTrade

alter table RabbitMessages add status [varchar](10)
alter table RabbitMessages add result [varchar](1024)
alter table RabbitMessages add errorMsg [varchar](1024)

alter table RabbitMessages add tradeInsertedCount [int]
alter table RabbitMessages add tradeUpdatedCount [int]
alter table RabbitMessages add tradeDeletedCount [int]



