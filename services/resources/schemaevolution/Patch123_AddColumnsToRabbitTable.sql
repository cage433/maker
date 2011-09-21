truncate table RabbitMessages

alter table TitanTrade add payloads [varchar](max) NULL
alter table TitanTrade add starlingTimestamp [datetime]  NULL
