package starling.services
package trade

import instrumentreaders.ExcelInstrumentReader
import starling.utils.Reflection
import starling.instrument._
import starling.instrument.Trade
import starling.eai.{EAIStrategyDB, Traders}
import collection.mutable.ArraySeq
import starling.daterange.Day
import starling.market.{CommodityMarket, FuturesSpreadMarket}
import starling.auth.User
import starling.concurrent.MP._
import starling.eai.{EAIDealBookMapping, EAIStrategyDB, Traders}

class ExcelTradeReader(eaiStrategyDB: EAIStrategyDB, eaiDealBookMapping: EAIDealBookMapping, traders: Traders, currentlyLoggedOn: () => User) {
  import ExcelRow._

  def allTrades(header: Array[String], trades: List[Seq[Object]], subgroupNamePrefix: String): List[Trade] = {
    val empties = (" " * 20).split("")
    allTrades(trades.map(tradeValues => header.zip(tradeValues ++ empties).toMap), subgroupNamePrefix)
  }

  def allTrades(originalRows: List[Map[String, Object]], subgroupNamePrefix: String): List[Trade] = {
    allTrades0(originalRows, subgroupNamePrefix).reverse
  }

  private def allTrades0(originalRows: List[Map[String, Object]], subgroupNamePrefix: String):List[Trade] = {
    val currentUser = currentlyLoggedOn()

    // non-blank rows, with the key in lower case
    val rows: List[Map[String, Object]] = originalRows.map{
      m => m.map{
        case (k, v) => k.toLowerCase -> v
      }
    }.filterNot{
      m => m.filterKeys(_ != TradeIDColumn).values.forall(_.toString.trim.isEmpty)
    }

    // rows that have 2 strategies get split into 2 rows, one trade in each strategy
    val CounterPartySpread = """(\d+)[ ]*/[ ]*(\d+)""".r
    val explodedRows = rows.flatMap {
      map => map(StrategyColumn) match {
        case CounterPartySpread(c1, c2) => {
          val row = new ExcelRow(map, traders, currentUser)
          val tradeID = row.formattedExcelColumnID

          // t1 is the same, just add 'a' to the row id
          val t1 = map + (StrategyColumn -> c1, TradeIDColumn -> (tradeID + "a"))

          // t2 has 'b' after its row id and the opposite volume
          val volume = row.volumeDouble * -1
          val t2 = map + (ExcelRow.VolumeColumn -> volume, StrategyColumn -> c2, TradeIDColumn -> (tradeID + "b"))
          
          List(t1, t2)
        }
        case _ => List(map)
      }
    }

    val dups = explodedRows.map(ExcelRow(_, traders, currentUser).formattedExcelColumnID).groupBy(a => a).filter(_._2.size > 1).keys
    assert(dups.isEmpty, "Duplicate IDs aren't allowed: " + dups.mkString(", "))

    explodedRows.map {
      row =>
      val excelRow = ExcelRow(row, traders, currentUser)
      try {
        val tradeDay = excelRow.tradeDay
        val counterParty = excelRow.counterParty

        val tradeAttributes = excelRow.attributes(eaiStrategyDB, eaiDealBookMapping, subgroupNamePrefix)
        val tradeID = excelRow.tradeID(tradeAttributes.subgroupName)
        val trade = new Trade(tradeID, tradeDay, counterParty, tradeAttributes, ExcelTradeReader.instrument(excelRow),
          ExcelTradeReader.readCosts(excelRow))

        // this test will just try to get the price and vol keys for the entered trade, if it throws an
        // exception it will throw an error here rather than when trying to value it later
        val testDay = Day(2011, 1, 1).endOfDay
        trade.asUtpPortfolio.portfolio.keys.map {
          utp => utp.priceAndVolKeys(testDay)
        }
        
        trade
      }
      catch {
        case e => {
          val eClass = e.getClass.toString.split('.').last
          throw new Exception("Error on row with ID '" + excelRow.excelColumnID + "': " + eClass + ":" + e.getMessage, e)
        }
      }
    }
  }

}

object ExcelTradeReader {
  lazy val readers = Reflection.listClassesOfType("starling.services.trade.instrumentreaders", classOf[ExcelInstrumentReader]).map(c => c.newInstance)

  def instrument(excelRow: ExcelRow): Tradeable = {
    try {
      assert(readers.nonEmpty, "No readers defined")
      readers.filter(r => r.canRead(excelRow)) match {
        case reader :: Nil => reader.create(excelRow)
        case Nil => throw new Exception("No readers matched: " + excelRow)
        case r => throw new Exception("Too many readers (" + r + ") matched: " + excelRow)
      }
    } catch {
      case m: MatchError => throw new Exception("A broken reader is throwing a match error instead of returning false", m)
    }
  }

  def readCosts(row: ExcelRow): List[Costs] = row.instrumentType match {
    case FuturesOption | AsianOption | CalendarSpreadOption => {
      val premium = row.price
      assert(premium.isPositve, "Premiums should be positive: " + premium)
      val cp = row.counterParty
      val tradeDay = row.tradeDay
      val settlementDay = row.market match {
        case m: FuturesSpreadMarket => m.premiumSettlementDay(tradeDay)
        case m: CommodityMarket => m.premiumSettlementDay(tradeDay)
      }
      List(new PremiumCosts(settlementDay, cp, row.volume, premium))
    }
    case _ => Nil
  }
}
