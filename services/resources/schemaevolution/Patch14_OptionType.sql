truncate table EAITrade
alter table EAITrade add optionType nvarchar(15)
alter table TrinityTrade add optionType nvarchar(15)
alter table GalenaTrade add optionType nvarchar(15)
-- alter table Instrument add optionType nvarchar(15)