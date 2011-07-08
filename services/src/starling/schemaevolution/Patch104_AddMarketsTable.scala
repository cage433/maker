package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.calendar.BrentMonth

class Patch104_AddMarketsTable extends Patch {
  val names = (1 to 12).map {
    i => {
      val month = new BrentMonth(i)
      val old = "Platts Brent " + month.monthName
      val newname = "Platts Brent (" + month.monthName + ")"
      (old, newname)
    }
  }.toMap


  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    // first do a conversion of some old market names
    names.map {
      case (oldName, newName) => {
        writer.queryForUpdate("select marketDataKey from MarketData where marketDataKey like '%>" + oldName + "<%'") {
          rs => {
            val key: String = rs.getString("marketDataKey")
            val newKey = key.replace(oldName, newName)
            rs.update(Map("marketDataKey" -> newKey))
          }
        }
      }
      val tables = List("EAITrade", "IntradayTrades")
      tables.map(table => writer.update("update " + table + " set market = :newMarket where market = :oldMarket", Map("newMarket" -> newName, "oldMarket" -> oldName)))
    }

    writer.update("""
CREATE TABLE [dbo].[Markets](
	[Name] [varchar](255) NOT NULL,
	[eaiQuoteID] [nchar](20) NOT NULL,
	[type] [varchar](50) NOT NULL,
	[uom] [nchar](20) NULL,
	[ccy] [nchar](20) NULL,
	[businessCalendar] [varchar](255) NULL,
	[lotSize] [nchar](20) NULL,
	[tenor] [nchar](20) NULL,
	[Commodity] [varchar](255) NULL,
	[limSymbol] [varchar](255) NULL,
	[limMultiplier] [nchar](20) NULL,
	[defaultPrecision] [nchar](20) NULL,
	[clearPortPrecision] [nchar](20) NULL,
	[expiryRule] [varchar](255) NULL,
	[exchange] [varchar](255) NULL,
	[volatilityID] [nchar](20) NULL,
	[indexLevel] [varchar](255) NULL,
	[forwardMarket] [varchar](255) NULL,
	[pricingRule] [varchar](255) NULL,
	[formula] [varchar](1024) NULL,
	[rollbefore] [tinyint] NULL,
	[promptness] [tinyint] NULL,
	[bblPerMT] [nchar](20) NULL,
 CONSTRAINT [PK_markets] PRIMARY KEY CLUSTERED
(
	[Name] ASC
)
) ON [PRIMARY]
  """
    )

    def int(s: String) = if (s.isEmpty) null else s.toInt
    def out(s: String) = if (s.isEmpty) null else s

    val markets: List[String] = metadata.Patch104Markets.markets
    val header = markets.head.split('\t').map(_.toLowerCase).zipWithIndex.toMap
    val lines = markets.tail.map {
      line => {
        val entries = (line + "\t|").split('\t')
        try {
          val insert = header.map {
            case ("rollbefore", i) => ("rollbefore" -> int(entries(i).trim))
            case ("promptness", i) => ("promptness" -> int(entries(i).trim))
            case (name, i) => (name -> out(entries(i).trim))
          }.toMap
          writer.insert("Markets", insert)
        }
        catch {
          case e => {
            throw new Exception("Failed to insert: " + entries.toList, e)
          }
        }
      }
    }
  }

  def patchDescription = "add markets table"
}
