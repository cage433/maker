truncate table RabbitMessages

alter table RabbitMessages add payloads [varchar](max) NULL
alter table RabbitMessages add starlingTimestamp [datetime]  NULL
