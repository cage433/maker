package starling.market

import rules.{MarketPrecisionFactory, PrecisionRules}
import starling.calendar.{HolidayTablesFactory, BusinessCalendars}
import concurrent.stm._
import starling.utils.ImplicitConversions._

trait MarketLookup {
  def allMarkets:List[Market]

  def allIndexes:List[Index]

  /**
   * This should be called if markets change
   */
  def changed() = atomic {
    implicit txn => stored() = Stored(allMarkets, allIndexes)
  }

  private lazy val stored = Ref(Stored(allMarkets, allIndexes))

  private case class Stored(allMarkets:List[Market], allIndexes:List[Index]) {
    lazy val marketNameMap = allMarkets.map(m => m.name.toLowerCase -> m).toMap

    lazy val marketEAIMap = allMarkets.flatMap {
      case m:CommodityMarket => m.eaiQuoteID.map(_ -> m)
      case _ => None
    }.toMap

    lazy val indexNameMap = allIndexes.map(m => m.name.toLowerCase -> m).toMap
    lazy val indexEAIMap:Map[Int, Index] = {
      // all the index quote ids plus the futures markets quote ids pointing the the front period indexes
      // this is because swaps and formula indexes are sometimes (incorrectly) against the futures market id
      allIndexes.flatMap(i => i.eaiQuoteID.map((_, i))) :::
        allIndexes.flatMap {
          case fi:FuturesFrontPeriodIndex if fi.promptness == 1 => fi.market.eaiQuoteID.map((_, fi))
          case _ => None
        } toMap
    }

    lazy val allPublishedIndexes = allIndexes.flatMap {
      case p:PublishedIndex => Some(p);
      case _ => None
    }

    lazy val marketToPublishedIndexMap:Map[CommodityMarket, PublishedIndex] = allPublishedIndexes.toMapWithKeys(_.market)

    lazy val marketToFrontPeriodIndexMap:Map[FuturesMarket, FuturesFrontPeriodIndex] = allIndexes.flatMap {
      case index@FuturesFrontPeriodIndex(_, _, market, _, 1, _) => Some(market -> index)
      case _=> None
    }.toMap
  }

  protected[market] def futuresMarket(name:String):Option[FuturesMarket] = getAsType(stored.single().marketNameMap, name.toLowerCase, classOf[FuturesMarket])

  protected[market] def futuresMarket(eaiQuoteID:Int):Option[FuturesMarket] = getAsType(stored.single().marketEAIMap, eaiQuoteID, classOf[FuturesMarket])

  protected[market] def futuresSpreadMarket(name:String):Option[FuturesSpreadMarket] = getAsType(stored.single().marketNameMap, name.toLowerCase, classOf[FuturesSpreadMarket])

  protected[market] def futuresSpreadMarket(eaiQuoteID:Int):Option[FuturesSpreadMarket] = getAsType(stored.single().marketEAIMap, eaiQuoteID, classOf[FuturesSpreadMarket])

  private def getAsType[K, V <: AnyRef, R](map:Map[K, V], key:K, klass:Class[_]):Option[R] = map.get(key) match {
    case Some(v) if klass.isAssignableFrom(v.getClass) => Some(v.asInstanceOf[R])
    case None => None
    case Some(v) => throw new Exception("Not the right type: " +(v, v.getClass, klass))
  }

  protected[market] def index(name:String):Option[Index] = stored.single().indexNameMap.get(name.toLowerCase)

  protected[market] def index(eaiQuoteID:Int):Option[Index] = stored.single().indexEAIMap.get(eaiQuoteID)

  protected[market] def allFuturesMarkets:List[FuturesMarket] = allMarkets.flatMap {
    case f:FuturesMarket => Some(f)
    case _ => None
  }

  protected[market] def getPublishedIndexForMarket(market:CommodityMarket):Option[PublishedIndex] = stored.single().marketToPublishedIndexMap.get(market)

  protected[market] def futuresMarketToIndex(market: FuturesMarket): Option[FuturesFrontPeriodIndex] = stored.single().marketToFrontPeriodIndexMap.get(market)
}

trait MarketLookupCreator {
  /**
   * This can be called multiple times (e.g. when a reload is needed)
   */
  def create:MarketLookup
}

object MarketProvider {

  import concurrent.stm._

  private val creator = Ref(None:Option[MarketLookupCreator])
  private val impl = Ref(None:Option[MarketLookup])

  def registerCreator(c:MarketLookupCreator) {
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

  def testRegisterCreator(c:MarketLookupCreator) { // for tests
    atomic {
      implicit txn => {
        println("Registering from ")
        (new Exception).printStackTrace()
        creator() = Some(c)
        impl() = Some(c.create)
      }
    }
  }

  def provider:MarketLookup = atomic {
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
