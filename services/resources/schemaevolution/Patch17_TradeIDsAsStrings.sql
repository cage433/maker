truncate table EAITrade
alter table  EAITrade add
 fixedRate float, fixedBasis varchar(50), floatBasis varchar(50),
 fixedPaymentFreq varchar(50), floatPaymentFreq varchar(50) 

EXEC sp_rename
@objname = 'EAITrade.averagingPeriodStart',
@newname = 'startDate',
@objtype = 'COLUMN'

EXEC sp_rename
@objname = 'EAITrade.averagingPeriodEnd',
@newname = 'endDate',
@objtype = 'COLUMN'

ALTER TABLE EAITrade
ALTER COLUMN tradeid char (10) NOT NULL

drop index GalenaTrade.NUC_GalenaTrade
ALTER TABLE GalenaTrade
ALTER COLUMN tradeid char (10) NOT NULL
CREATE CLUSTERED INDEX NUC_GalenaTrade on GalenaTrade(timestampto_cache, instrument, portfolio, tradeid)

drop index TrinityTrade.NUC_TrinityTrade
ALTER TABLE TrinityTrade
ALTER COLUMN tradeid char (10) NOT NULL
CREATE CLUSTERED INDEX NUC_TrinityTrade on TrinityTrade(timestampto_cache, instrument, portfolio, tradeid)
