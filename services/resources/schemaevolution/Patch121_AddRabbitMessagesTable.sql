CREATE TABLE [dbo].[RabbitMessages](
                [starlingID] [bigint] NOT NULL,
                [verb] [varchar](300) NULL,
                [subject] [varchar](300) NULL,
                [id] [varchar](300) NULL,
                [source] [varchar](300) NULL,
                [timestamp] [datetime] NULL,
                [host] [varchar](300) NULL,
                [pid] [int] NULL,
                [body] [varchar](max) NULL,
CONSTRAINT [PK_RabbitMessages] PRIMARY KEY CLUSTERED
(
                [starlingID] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]