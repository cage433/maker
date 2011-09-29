package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import starling.utils.Log
import starling.gui.api.{EAIDeskInfo, Desk}

class Patch122_MoveEAITradeTableIntoTablesForEachBook extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    starling.inTransaction {
      writer => {
        Desk.eaiDesks.map {
          case Desk(_, _, Some(info: EAIDeskInfo)) => {
            val bookID = info.book
            val tableName = "EAITrade_book_" + bookID
            Log.infoWithTime("Creating new table for book " + bookID) {
              val createTableSQL = creatTable(tableName)
              writer.update(createTableSQL)
              val sql = insert(tableName) + select(bookID)
              writer.withIdentityInsert(tableName) {
                writer.update(sql)
              }
            }
          }
        }
        writer.update("exec sp_rename 'EAITrade', 'OldEAITrade'")
      }
    }
  }

  def insert(name: String) = {
    """
      INSERT INTO """ + name + """
           ([ID],
            [tradeID]
           ,[tradeDay]
           ,[counterParty]
           ,[instrument]
           ,[quantity]
           ,[quantityUOM]
           ,[strike]
           ,[strikeUOM]
           ,[spread]
           ,[spreadUOM]
           ,[market]
           ,[lastTradingDay]
           ,[deliveryDay]
           ,[exerciseDay]
           ,[maturityDay]
           ,[callPut]
           ,[timestamp]
           ,[error]
           ,[dealID]
           ,[strategyID]
           ,[Period]
           ,[exerciseType]
           ,[timestampTo_cache]
           ,[nextVersionId_cache]
           ,[expiryDay_cache]
           ,[fixedRate]
           ,[costs]
           ,[trader]
           ,[tradedFor]
           ,[broker]
           ,[clearinghouse]
           ,[cleared]
           ,[InitialPrice]
           ,[InitialPriceUOM]
           ,[PricingRule]
           ,[CashInstrumentType])
      """
  }

  def select(bookID: Int) = {
    """
    SELECT [id]
        ,[tradeID]
        ,[tradeDay]
        ,[counterParty]
        ,[instrument]
        ,[quantity]
        ,[quantityUOM]
        ,[strike]
        ,[strikeUOM]
        ,[spread]
        ,[spreadUOM]
        ,[market]
        ,[lastTradingDay]
        ,[deliveryDay]
        ,[exerciseDay]
        ,[maturityDay]
        ,[callPut]
        ,[timestamp]
        ,[error]
        ,[dealID]
        ,[strategyID]
        ,[Period]
        ,[exerciseType]
        ,[timestampTo_cache]
        ,[nextVersionId_cache]
        ,[expiryDay_cache]
        ,[fixedRate]
        ,[costs]
        ,[trader]
        ,[tradedFor]
        ,[broker]
        ,[clearinghouse]
        ,[cleared]
        ,[InitialPrice]
        ,[InitialPriceUOM]
        ,[PricingRule]
        ,[CashInstrumentType]
    FROM EAITrade old
    where old.bookID = """ + bookID
  }

  def creatTable(name: String) = {
    """
  CREATE TABLE [dbo].[""" + name + """](
    [id] [int] IDENTITY(1,1) NOT NULL,
    [tradeID] [char](10) NOT NULL,
    [tradeDay] [datetime] NULL,
    [counterParty] [varchar](255) NULL,
    [instrument] [varchar](50) NULL,
    [quantity] [float] NULL,
    [quantityUOM] [char](20) NULL,
    [strike] [varchar](50) NULL,
    [strikeUOM] [char](20) NULL,
    [spread] [float] NULL,
    [spreadUOM] [char](20) NULL,
    [market] [varchar](255) NULL,
    [lastTradingDay] [datetime] NULL,
    [deliveryDay] [datetime] NULL,
    [exerciseDay] [datetime] NULL,
    [maturityDay] [datetime] NULL,
    [callPut] [char](1) NULL,
    [timestamp] [datetime] NULL,
    [error] [varchar](max) NULL,
    [dealID] [int] NULL,
    [strategyID] [int] NULL,
    [Period] [varchar](255) NULL,
    [exerciseType] [nvarchar](15) NULL,
    [timestampTo_cache] [datetime] NULL,
    [nextVersionId_cache] [int] NULL,
    [expiryDay_cache] [datetime] NULL,
    [fixedRate] [float] NULL,
    [costs] [varchar](8000) NULL,
    [trader] [varchar](50) NULL,
    [tradedFor] [varchar](50) NULL,
    [broker] [varchar](50) NULL,
    [clearinghouse] [varchar](50) NOT NULL,
    [cleared] [tinyint] NULL,
    [InitialPrice] [varchar](50) NULL,
    [InitialPriceUOM] [varchar](20) NULL,
    [PricingRule] [varchar](25) NULL,
    [CashInstrumentType] [varchar](255) NULL,
   CONSTRAINT [PK_""" + name + """_id] PRIMARY KEY CLUSTERED
  (
    [id] ASC
  )WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
  ) ON [PRIMARY]

  SET ANSI_PADDING OFF
    """
  }
}