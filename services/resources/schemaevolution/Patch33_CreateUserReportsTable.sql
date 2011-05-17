CREATE TABLE [dbo].[UserReports](
 [starlingUser] [varchar](30) NOT NULL,
 [reportName] [varchar](30) NOT NULL,
 [layoutName] [varchar](30) NOT NULL,
 [report] [text] NOT NULL,
 CONSTRAINT [PK_UserReports] PRIMARY KEY CLUSTERED
(
 [starlingUser] ASC,
 [reportName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]