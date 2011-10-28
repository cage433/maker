package starling.market

import rules.{MarketPrecisionFactory, PrecisionRules}
import starling.calendar.{HolidayTablesFactory, BusinessCalendars}

trait MarketLookup {
  def futuresMarket(name: String): Option[FuturesMarket] = getAsType(marketNameMap, name.toLowerCase, classOf[FuturesMarket])

  def futuresMarket(eaiQuoteID: Int): Option[FuturesMarket] = getAsType(marketEAIMap, eaiQuoteID, classOf[FuturesMarket])

  def futuresSpreadMarket(name: String): Option[FuturesSpreadMarket] = getAsType(marketNameMap, name.toLowerCase, classOf[FuturesSpreadMarket])

  def futuresSpreadMarket(eaiQuoteID: Int): Option[FuturesSpreadMarket] = getAsType(marketEAIMap, eaiQuoteID, classOf[FuturesSpreadMarket])

  private def getAsType[K,V <: AnyRef, R](map: Map[K, V], key: K, klass: Class[_]): Option[R] = map.get(key) match {
    case Some(v) if klass.isAssignableFrom(v.getClass) => Some(v.asInstanceOf[R])
    case None => None
    case Some(v) => throw new Exception("Not the right type: " + (v, v.getClass, klass))
  }

  def index(name: String): Option[Index] = indexNameMap.get(name.toLowerCase)

  def index(eaiQuoteID: Int): Option[Index] = indexEAIMap.get(eaiQuoteID)

  // The presence of 'London Close' markets meant that the pair exchange/commodity could map to more than one market
  protected def allMarketsImpl: List[Market]
  final val allMarkets: List[Market] = allMarketsImpl.filterNot(_.name.contains("London close"))

  lazy val allFuturesMarkets: List[FuturesMarket] = allMarkets.flatMap{
    case f:FuturesMarket => Some(f)
    case _ => None
  }

  lazy val marketNameMap = allMarkets.map(m => m.name.toLowerCase -> m).toMap

  lazy val marketEAIMap = allMarkets.flatMap {
    case m: CommodityMarket => m.eaiQuoteID.map(_ -> m)
    case _ => None
  }.toMap

  val allIndexes: List[Index]
  lazy val indexNameMap = allIndexes.map(m => m.name.toLowerCase -> m).toMap
  lazy val indexEAIMap: Map[Int, Index] = allIndexes.flatMap(m => m.eaiQuoteID.map(_ -> m)).toMap
}

trait MarketLookupCreator {
  /**
   * This can be called multiple times (e.g. when a reload is needed)
   */
  def create: MarketLookup
}

object MarketProvider {
  import concurrent.stm._

  private val creator = Ref(None: Option[MarketLookupCreator])
  private val impl = Ref(None: Option[MarketLookup])

  def registerCreator(c: MarketLookupCreator) {
    atomic {
      implicit txn =>
        creator() match {
          case None => {
            creator() = Some(c)
            impl() = Some(c.create)
          }
          case Some(currentMarketLookup) if c == currentMarketLookup =>
          case _ => throw new Exception("Market provider creator implementation already registered")
        }
    }
  }

  def provider: MarketLookup = atomic {
    implicit txn =>
      impl() match {
        // if you get this when running a test change the test to extend TestMarketSpec
        case None => throw new Exception("Market provider implementation not yet registered")
        case Some(i) => i
      }
  }

  /**
   * this should only be called from inside patches that change the markets
   */
  def reload = atomic {
    implicit txn =>
      creator() match {
        case Some(c) => impl() = Some(c.create)
        case _ => 
      }
  }
}
