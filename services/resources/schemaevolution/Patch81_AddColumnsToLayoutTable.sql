alter table [dbo].[PivotLayouts]
 add [layoutType] [varchar](30) constraint DF_PivotLayouts_layoutType default 'Report' NOT NULL

alter table [dbo].[PivotLayouts]
 add [associatedReport] text