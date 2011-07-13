package starling.services.trade

import instrumentreaders.{ExcelInstrumentReaderException, ExcelInstrumentReader}
import starling.models._
import starling.instrument._
import starling.daterange._
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.db.IntradayTradeSystem
import starling.auth.User
import starling.trade.TradeID
import starling.eai.{Traders, TreeID, EAIStrategyDB}
import starling.market.rules.{CommonPricingRule, SwapPricingRule}
import starling.quantity.{UOM, Quantity}
import starling.quantity.UOM._
import starling.utils.{StringToInt, StringToDouble}
import starling.utils.CollectionUtils._
import starling.market._

case class ExcelRow(row: Map[String, Any], traders: Traders) {

  import ExcelInstrumentReader._
  import ExcelRow._

  assert(row.contains(TradeIDColumn), "No ID column, or wrong name, should be called: " + TradeIDColumn)
  assert(row.contains(VolumeColumn), "No Volume column, or wrong name, should be called: " + VolumeColumn)
  assert(row.contains(UnitColumn), "No unit column, or wrong name, should be called: " + UnitColumn)
  assert(row.contains(InstrumentColumn), "No Instrument column, or wrong name, should be called: " + InstrumentColumn)
  assert(row.contains(MarketColumn), "No Market column, or wrong name, should be called: " + MarketColumn)
  assert(row.contains(PeriodColumn), "No volume column, or wrong name, should be called: " + PeriodColumn)
  checkMarket
  checkInstrument

  val sys = IntradayTradeSystem

  /**
   * returns the column 'id' from excel as a string
   */
  def excelColumnID = {
    string(TradeIDColumn) match {
      case StringToInt(i) => i.toString
      case StringToDouble(d) if d.toInt == d => d.toInt.toString
      case s => s
    }
  }

  /**
   * returns the formatted column 'id' from excel as a string
   * numbers are formatted to use 4 digits
   * strings are left as is
   */
  def formattedExcelColumnID = {
    val formattedID = string(TradeIDColumn) match {
      case TradeIDRegex(id) => id match {
        case StringToInt(i) => i.formatted("%04d")
        case StringToDouble(d) if d.toInt == d => d.toInt.formatted("%04d")
        case s if !s.trim.isEmpty => s

      }
      case o => throw new ExcelInstrumentReaderException("Invalid excelColumnID: '" + o + "', try using only numbers or letters")
    }
    formattedID
  }

  val TradeIDRegex = """([\w\d\.-]+)""".r
  def tradeID(subgroupName: String) = {
    val formattedID = formattedExcelColumnID
    TradeID(subgroupName + "-" + formattedID, sys)
  }

  def instrumentType = {
    val instr = string(InstrumentColumn)
    instrumentAliases.get(instr.toLowerCase) match {
      case Some(i) => i
      case None => InstrumentType.fromName(instr).get
    }
  }

  def volumeDouble = double(VolumeColumn)

  def volume = {
    val size = volumeDouble
    val volumeUOM = marketPriceUOM.denominatorUOM
    string(UnitColumn).toLowerCase match {
      case "lot" | "lots" => {
        Quantity(size * lotSize, volumeUOM)
      }
      case UOM.Parse(uom) => {
        // try to convert things to 'kb' to 1000 bbls, and 'kt' to 1000 mts. the conversion
        // won't try to convert things like bbls into mt etc as we want to preserve those units.
        val q = fromLots(Quantity(size, uom))
        q
      }
      case o => throw new ExcelInstrumentReaderException("Unknown unit: " + o)
    }
  }

  private def fromLots(q: Quantity): Quantity = (q in fromLots(q.uom)) get

  private def fromLots(uom: UOM): UOM = uom match {
    case K_BBL => BBL
    case C_MT => MT
    case K_MT => MT
    case C_M3 => M3
    case _ => uom
  }

  def strike = {
    val q = Quantity(double(StrikeColumn), priceUOM)
    instrumentType match {
      case CalendarSpreadOption => q
      case i if q.isNegative => throw new ExcelInstrumentReaderException("Can't have negative strike on " + i)
      case _ => q
    }
  }

  def price = {
    val name = marketStr
    val multiIndex = indexAliases.get(name) match {
      case Some(index: MultiIndex) => true
      case _ => false
    }
    val spread = period match {
      case _: SpreadPeriod => true
      case _ => false
    }
    val crack = isSpreadMarket

    val q = Quantity(double(PriceColumn), priceUOM)
    if (!multiIndex && !spread && !crack)
      assume(q >= 0, "Price can't be negative for non spread trades: " + q)
    q
  }

  def prices: List[Quantity] = {
    row.get(PriceColumn) match {
      case Some(v: String) if v.contains("/") => {
        v.split("/").map(d => {
          val q = Quantity(doubleFromString(d.toString, PriceColumn), priceUOM)
          assume(q >= 0, "Can't have a negative price when both legs of a spread specified: " + q)
          q
        }).toList
      }
      case Some(v) => List(price)
      case None => List()
    }
  }

  def callOrPut = string(CallPutColumn) match {
    case CallOrPut(cp) => cp
    case _ => throw new Exception("Couldn't parse '" + string(CallPutColumn) + "' as call/put")
  }

  def exerciseType: Option[ExerciseType] = {
    row.get(ExerciseColumn) match {
      case Some(s: String) if s.trim.isEmpty => None
      case Some(s: String) if !s.trim.isEmpty => s match {
        case ExerciseType(e) => Some(e)
        case _ => throw new ExcelInstrumentReaderException("Invalid exercise type: " + s)
      }
      case o => throw new ExcelInstrumentReaderException("Invalid exercise type: " + o)
    }
  }

  def marketStr = string(MarketColumn, 100).toLowerCase

  def market = marketAliases.get(marketStr) match {
    case Some(m) => m
    case None => throw ExcelInstrumentReaderException("Couldn't find market with name: " + marketStr)
  }

  def isSpreadMarket = marketAliases.get(marketStr) match {
    case Some(m: FuturesSpreadMarket) => true
    case None => false
  }

  def futuresSpreadMarket: FuturesSpreadMarket = marketAliases(marketStr).asInstanceOf[FuturesSpreadMarket]

  def futuresMarket = market.asInstanceOf[FuturesMarket]

  def index = indexAliases.get(marketStr) match {
    case Some(i) => i
    case None => {
      marketAliases.get(marketStr) match {
        case Some(market) => {
          val matches = Index.singleIndexes.filter(_.market == market).mkString(" or ")
          throw ExcelInstrumentReaderException("Couldn't find index with name: " + marketStr + ", did you mean '" + matches + "'?")
        }
        case _ => throw ExcelInstrumentReaderException("Couldn't find index with name: " + marketStr)
      }

    }
  }


  def singleIndex = index.asInstanceOf[SingleIndex]

  def lotSize = {
    val name = marketStr
    if (isSpreadMarket) {
      futuresSpreadMarket.lotSize
    } else {
      val lots = (marketAliases.get(name), indexAliases.get(name)) match {
        case (Some(market: CommodityMarket), None) => market.lotSize
        case (None, Some(index: SingleIndex)) => index.lotSize
        case (Some(market: CommodityMarket), Some(index: SingleIndex)) if market == index.market => market.lotSize
        case _ => throw new Exception("Can't figure out lot size for: " + name)
      }
      lots match {

        case Some(l) => l
        case None => throw new Exception("Lot size not defined for: " + name)
      }
    }
  }

  def rawPeriod: String = string(PeriodColumn)

  def period: Period = {
    row(PeriodColumn) match {
      case Period(p) => p match {
        case DateRangePeriod(d: Day) => {
          dayCheck(d, PeriodColumn)
          d
        }
        case _ => p
      }
      case s: String if s.equalsIgnoreCase("BOM") => BOM(tradeDay)
      case _ => throw new Exception("Couldn't parse '" + string(PeriodColumn) + "' in column " + PeriodColumn)
    }
  }

  protected def string(name: String, maxLength: Int = 50) = {
    val s = row(name).toString.trim
    assume(s.length <= maxLength, name + " field too long, max: " + maxLength)
    s
  }

  protected def stringOrEmptyIfMissing(name: String, maxLength: Int = 50) = {
    row.get(name) match {
      case Some(a) => {
        val s = a.toString.trim
        assume(s.length <= maxLength, name + " field too long, max: " + maxLength)
        s
      }
      case _ => ""
    }
  }

  protected def excelDay(d: Double, name: String) = {
    val day = Day.fromExcel(d)
    dayCheck(day, name)
    day
  }

  protected def dayCheck(day: Day, name: String) {
    assume(day.year > 2000 && day.year < 2100, "Day in column '" + name + "' outside sensible date range: " + day)
  }

  protected def day(name: String): Day = row(name) match {
    case d: Double => excelDay(d, name)
    case StringToDouble(d) => excelDay(d, name)
    case s: String => Day.parse(s)
    case s => throw new Exception("Couldn't parse " + s + "' in column " + name)
  }

  protected def doubleFromString(str: String, column: String) = try {
    str.toDouble
  } catch {
    case n: NumberFormatException => throw new NumberFormatException("Failed to parse '" + str + "' as double for column " + column)
  }

  protected def double(name: String) = doubleFromString(row(name).toString, name)

  def checkMarket {
    val name = marketStr
    val all = (marketAliases.keys ++ indexAliases.keys).toSet
    if (!all.contains(name)) {
      val closest = closestLevenshteinString(all, name, 4)

      if (closest.nonEmpty) {
        val matches = closest.mkString(" or ")
        throw new Exception("Unexpected market name: " + name + " maybe: '" + matches + "'?")
      } else {
        throw new Exception("Unexpected market name: " + name)
      }
    }
  }

  def checkInstrument {
    val name = string(InstrumentColumn)

    try {
      instrumentType
    } catch {
      case _ => {
        val names = InstrumentType.types.map(_.name.toLowerCase)
          val closest = closestLevenshteinString(names, name, 4).map(new String(_))
          if (closest.nonEmpty) {
            val matches = closest.mkString(" or ")
            throw new Exception("Unexpected instrument name: " + name + " maybe: '" + matches + "'?")
          } else {
            throw new Exception("Unexpected instrument name: " + name)
          }
      }
    }

  }

  def marketPriceUOM = {
    val name = marketStr
    if (isSpreadMarket) {
      futuresSpreadMarket.priceUOM
    } else {
      (marketAliases.get(name), indexAliases.get(name)) match {
        case (Some(market: CommodityMarket), None) => market.priceUOM
        case (None, Some(index)) => index.priceUOM
        case (Some(market: CommodityMarket), Some(index: SingleIndex)) if market == index.market => market.priceUOM
        case _ => throw new Exception("Unexpected market name: " + name)
      }
    }
  }

  def priceUOM = {
    marketPriceUOM
  }

  def tradeDay = day(TradeDateColumn)

  def entryDay = day(EntryDateColumn)

  def counterParty = string(CounterPartyColumn) match {
    case "" => "No CounterParty"
    case s => s
  }

  def broker = string(BrokerColumn) match {
    case "" => "No Broker"
    case s => s
  }

  def clearingHouse = string(ClearingHouseColumn)

  def pricingRule = stringOrEmptyIfMissing(PricingRuleColumn) match {
    case SwapPricingRule(r) => r
    case _ => CommonPricingRule
  }

  def comment = string(CommentColumn, 255)


  val bookIDs = traders.bookMap.map {
    case (user, (book, _)) => (user.name.toLowerCase -> TreeID(book.bookID))
  }


  def getStrategyFromDealId(eaiStrategyDB: EAIStrategyDB, userDealID: String): (Option[TreeID], Option[TreeID]) = {
    val (strategyID, dealID) = userDealID match {
      case empty: String if empty.trim.isEmpty => (None, None)
      case strategy => {
        val id = TreeID(doubleFromString(strategy.toString, StrategyColumn).toInt)

        (eaiStrategyDB.getStrategyFromDealId(id), Some(id))
      }
    }

    (strategyID, dealID)
  }

  def attributes(eaiStrategyDB: EAIStrategyDB, subgroupName: String) = {
    val r = getStrategyFromDealId(eaiStrategyDB, row(StrategyColumn).toString)

    val strategyID: Option[TreeID] = r._1
    val dealID: Option[TreeID] = r._2

    val trader = string(TraderColumn)
    val tradedFor = string(TradedForColumn) match {
      case empty: String if empty.trim.isEmpty => trader
      case s => s
    }
    assert(bookIDs.contains(trader.toLowerCase), "Don't recognised name of trader: '" + string(TraderColumn) + "', map used: " + bookIDs)

    val bookID = bookIDs.get(tradedFor.toLowerCase) match {
      case Some(id) => id
      case None => TreeID(0)
    }
    val broker = this.broker
    val comment = this.comment
    val clearer = this.clearingHouse

    val entryDate = string(EntryDateColumn) match {
      case "" => tradeDay
      case _ => entryDay
    }

    val username = User.currentlyLoggedOn.username

    new IntradayTradeAttributes(strategyID, bookID, dealID, trader, tradedFor, broker, comment, clearer, subgroupName, entryDate, username)
  }
}

object ExcelRow {
  // trade column names
  val TradeIDColumn = "id"
  val TradeDateColumn = "trade date"
  val StrategyColumn = "strategy"
  val CounterPartyColumn = "counterparty"
  val BrokerColumn = "broker"
  val ClearingHouseColumn = "clearing house"
  val PricingRuleColumn = "pricing rule"
  val TraderColumn = "trader"
  val TradedForColumn = "tradedfor"
  val CommentColumn = "comment"
  val EntryDateColumn = "entry date"

  val UnitColumn = "unit"
  val InstrumentColumn = "instr"
  val MarketColumn = "market"
  val PeriodColumn = "period"
  val PriceColumn = "price"

  val StrikeColumn = "strike"
  val ExerciseColumn = "ex"
  val CallPutColumn = "p/c"
  val VolumeColumn = "size"
}
