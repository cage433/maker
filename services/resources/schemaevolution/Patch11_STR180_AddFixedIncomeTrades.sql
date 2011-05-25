EXEC sp_rename
    @objname = 'TrinityTrade.averagingPeriodStart',
    @newname = 'startDate',
    @objtype = 'COLUMN'    

EXEC sp_rename
    @objname = 'TrinityTrade.averagingPeriodEnd',
    @newname = 'endDate',
    @objtype = 'COLUMN'
        
alter table  TrinityTrade add
 fixedRate float, fixedBasis varchar(50), floatBasis varchar(50),
 fixedPaymentFreq varchar(50), floatPaymentFreq varchar(50) 


EXEC sp_rename
    @objname = 'GalenaTrade.averagingPeriodStart',
    @newname = 'startDate',
    @objtype = 'COLUMN'

EXEC sp_rename
    @objname = 'GalenaTrade.averagingPeriodEnd',
    @newname = 'endDate',
    @objtype = 'COLUMN'

alter table  GalenaTrade add
 fixedRate float, fixedBasis varchar(50), floatBasis varchar(50),
 fixedPaymentFreq varchar(50), floatPaymentFreq varchar(50)
