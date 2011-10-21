CREATE TABLE [dbo].[EmailsSent](
    [hash] [varchar](32) NOT NULL,
    [timestamp] [datetime] NOT NULL,
    [sender] [varchar](300) NOT NULL,
    [recipient] [varchar](300) NOT NULL,
    [subject] [varchar](max) NOT NULL,
    [body] [varchar](max) NOT NULL,
    CONSTRAINT [PK_EmailsSent] PRIMARY KEY CLUSTERED ([hash] ASC)
        WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON)
        ON [PRIMARY]
) ON [PRIMARY]