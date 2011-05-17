CREATE TABLE [dbo].[PageViewLog](
[loginName] [varchar](30) NOT NULL,
[userName] [varchar](30) NOT NULL,
[text] [varchar](1000) NOT NULL,
[shortText] [varchar](30) NOT NULL,
[pageString] [varchar](8000) NOT NULL,
[time] [datetime] NULL
)