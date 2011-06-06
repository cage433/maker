package starling.richdb

import java.sql.ResultSet
import java.lang.String
import collection.immutable.Map
import starling.db._
import starling.instrument._
import javax.sql.DataSource
import starling.curves.interestrate.DayCount
import starling.quantity.{Quantity, UOM}
import starling.utils.sql.ConnectionParams
import starling.market._
import rules.SwapPricingRule
import scala.collection.JavaConversions._
import starling.models._
import starling.daterange.{Day, Spread, DateRange}
import starling.quantity.{SpreadQuantity, Quantity, UOM}
import starling.trade.TradeSystem

class RichDB(val dataSource : DataSource, factory : RichResultSetRowFactory) extends DBTrait[RichResultSetRow] {
  def this(connectionParams: ConnectionParams, factory : RichResultSetRowFactory) =
    this(connectionParams.dataSource, factory)

  def db = new DB(dataSource)

  def resultSetFactory = factory

  override def convertTypes(params: Map[String, Any]) = RichConversions.convertTypes(params, super.convertTypes _)
}

object RichConversions {
  private val Period = "(?i)(period)(_[0-9]+)?".r

  /**
   * Convert from scala map to java map. Also convert the types in the params map. If there are no specific
   * rich conversions available fall back to conversion provided by @param fallBack 
   */
  def convertTypes(params: Map[String, Any], fallBack: Map[String, Any] => java.util.Map[String, AnyRef]): java.util.Map[String, AnyRef] = {
    var convertedMap = scala.collection.mutable.Map[String, AnyRef]()
    for((key, value) <- params) {
      (key,value) match {
        case (_, c : CallOrPut) => convertedMap += (key -> c.toShortString)
        case (_, o : ExerciseType) => convertedMap += (key -> o.toString)
        case (_, m : CommodityMarket) => convertedMap += (key -> m.name)
        case (_, i : Index) => convertedMap += (key -> i.identifier)
        // "period" in our tables are varchars so we don't want days being converted to sql dates.
        case (Period(_, _), d : Day) => convertedMap +=  (key -> d.toString)
        case (_, d : Day) => convertedMap +=  (key -> d.toSqlDate)
        case (_, d : DateRange) => convertedMap +=  (key -> d.toString)
        case (_, s : Spread[_]) => convertedMap +=  (key -> s.toString)
        case _ => convertedMap ++= asScalaMap(fallBack(Map(key -> value)))
      }
    }
    convertedMap
  }
}

class RichResultSetRowFactory extends ResultSetRowFactoryTrait[RichResultSetRow] {
  def create(resultSet: ResultSet) = new RichResultSetRow(resultSet)
}

class RichResultSetRow(resultSet: ResultSet)
  extends ResultSetRow(resultSet)
  with RichInstrumentResultSetRow
{
  def getMarket(column: String): CommodityMarket = Market.fromName(getString(column))

  def getFuturesMarket(column: String) = Market.futuresMarketFromName(getString(column))

  def getFuturesSpreadMarket(column: String) = FuturesSpreadMarket.find(getString(column)) match {
    case Some(m) => m
    case None => throw new Exception("Not a recognised futures spread market: " + getString(column))
  }

  def getForwardMarket(column: String) = Market.forwardMarketFromName(getString(column))

  def getCommodityMarketFromTrinityCode(column: String): CommodityMarket = Market.fromTrinityCode(getString(column))

  def getFuturesMarketFromTrinityCode(column: String) = getCommodityMarketFromTrinityCode(column) match {
    case m: FuturesMarket => m
    case o => throw new Exception("Market " + o + " is not a FuturesMarket")
  }

  def getForwardMarketFromTrinityCode(column: String) = getCommodityMarketFromTrinityCode(column) match {
    case m: ForwardMarket => m
    case f: FuturesMarket => new ProxyForwardMarket(f) // TODO [15 Apr 2010] until we know what they mean in trinity this is a guess
  }

  def isForwardMarketFromTrinityCode(column: String) = getCommodityMarketFromTrinityCode(column) match {
    case m: ForwardMarket => true
    case o => false
  }

  def getFuturesMarketFromEAIQuoteID(column: String) = Market.futuresMarketFromEAIQuoteID(getInt(column))

  def getSingleIndexFromEAIQuoteID(column: String) = Index.singleIndexFromEAIQuoteID(getInt(column))

  def getIndexFromEAIQuoteID(column: String) = Index.indexFromEAIQuoteID(getInt(column))

  def getIndexFromName(column: String) = Index.fromName(getString(column))

  def isBuy = getCharacter("bs") == 'B'

  def isSell = !isBuy

  /**
   * Returns the amount column with the right UOM and sign (as given by Buy/Sell)
   * The params indicate what the column names are.
   */
  def getAmount(amount: String, unit: String):Quantity = getAmount(amount, getUOM(unit))

  def getAmount(amount: String, uom: UOM):Quantity = {
    val volume = getDouble(amount)
    isSell match {
      case true => Quantity(-volume, uom)
      case false => Quantity(volume, uom)
    }
  }

  def getStrikes = {
    SpreadQuantity.parse(getString("InitialPrice") + " " + getString("InitialPriceUOM")) match {
      case Some(s) => s
      case None => throw new Exception("Couldn't parse: " + getString("strike"))
    }
  }

  def getCallPut(column: String) = {
    getString(column).head.toUpper match {
      case 'C' => Call
      case 'P' => Put
      //@TODO [17 Mar 2010] find out if these are right
      case 'R' => Call
      case 'Y' => Put
      case _ => throw new Exception("Bad Call/Put value: " + getString(column))
    }
  }

  def getDayCount(column : String) = DayCount.fromTrinityCode(getString(column))
}
