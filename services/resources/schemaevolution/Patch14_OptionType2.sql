update TrinityTrade
set optiontype = 'European'
where instrument like '%Option'
update TrinityTrade
set optiontype = 'Asian'
where instrument = 'Asian Option'

update GalenaTrade
set optiontype = 'European'
where instrument like '%Option'
update GalenaTrade
set optiontype = 'Asian'
where instrument = 'Asian Option'

-- update Instrument
-- set optiontype = 'European'
-- where instrument like '%Option'
-- update Instrument
-- set optiontype = 'Asian'
-- where instrument = 'Asian Option'

alter table RevalSnapshot
drop constraint PK_revalsnapshot
ALTER TABLE RevalSnapshot
ALTER COLUMN tradeid char (10) NOT NULL
alter table RevalSnapshot
ADD CONSTRAINT PK_revalsnapshot PRIMARY KEY CLUSTERED (snapshotid ASC, tradeid ASC) ON [PRIMARY]

