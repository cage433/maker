CREATE TABLE [dbo].[Bookmarks](
 [starlingUser] [varchar](30) NOT NULL,
 [bookmarkName] [varchar](30) NOT NULL,
 [bookmark] [text] NOT NULL,
 CONSTRAINT [PK_Bookmarks] PRIMARY KEY CLUSTERED
(
 [starlingUser] ASC,
 [bookmarkName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]