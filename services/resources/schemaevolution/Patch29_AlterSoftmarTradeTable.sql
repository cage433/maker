truncate table SoftmarTrade
truncate table SoftmarTradeUtp

alter table SoftmarTrade drop column enteredBy
alter table SoftmarTrade add purpose [varchar](100) NULL
alter table SoftmarTrade add trader [varchar](100) NULL
alter table SoftmarTrade add contractno [varchar](100) NULL
