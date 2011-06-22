DROP TABLE [dbo].[Bookmarks]

CREATE TABLE [dbo].[Bookmarks](
 [starlingUser] [varchar](300) NOT NULL,
 [bookmarkName] [varchar](300) NOT NULL,
 [bookmark] [text] NOT NULL,
 CONSTRAINT [PK_Bookmarks] PRIMARY KEY CLUSTERED
(
 [starlingUser] ASC,
 [bookmarkName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

ALTER TABLE PivotLayouts
ALTER COLUMN starlingUser [varchar](300) NOT NULL

ALTER TABLE PivotLayouts
ALTER COLUMN layoutName [varchar](300) NOT NULL

ALTER TABLE usersettings
ALTER COLUMN starlingUser [varchar](300) NOT NULL