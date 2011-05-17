CREATE TABLE [dbo].[PivotLayouts](
        [id] [int] IDENTITY(1,1) NOT NULL,
        [starlingUser] [varchar](30) NOT NULL,
        [layout] [text] NOT NULL,
 CONSTRAINT [PK_PivotLayouts] PRIMARY KEY CLUSTERED 
(
        [id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]

) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

