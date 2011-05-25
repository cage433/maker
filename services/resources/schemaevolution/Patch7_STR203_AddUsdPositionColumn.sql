-- sql server can only add non-null columns to a non empty table if a default value is given
alter table  VarReport add netUSDPosition float not null default 0, netUSDPositionUOM nvarchar(51) not null default 'USD'

-- but we can remove the default constraint - wierd
declare @default_name varchar(256)

select @default_name = [name] from sys.default_constraints
where parent_object_id = object_id('VarReport')
and col_name(parent_object_id, parent_column_id) = 'netUSDPositionUOM'
exec('alter table dbo.VarReport drop constraint ' + @default_name)

select @default_name = [name] from sys.default_constraints
where parent_object_id = object_id('VarReport')
and col_name(parent_object_id, parent_column_id) = 'netUSDPosition'
exec('alter table dbo.VarReport drop constraint ' + @default_name)



