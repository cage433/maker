package starling.market

import rules.{MarketPrecisionFactory, PrecisionRules}
import starling.calendar.{HolidayTablesFactory, BusinessCalendars}

trait MarketLookup {
  def futuresMarket(name: String): Option[FuturesMarket] = marketNameMap.get(name.toLowerCase)
  def futuresMarket(eaiQuoteID: Int): Option[FuturesMarket] = marketEAIMap.get(eaiQuoteID)

  def index(name: String): Option[Index] = indexNameMap.get(name.toLowerCase)
  def index(eaiQuoteID: Int): Option[Index] = indexEAIMap.get(eaiQuoteID)

  val allFuturesMarkets: List[FuturesMarket]
  lazy val marketNameMap = allFuturesMarkets.map(m => m.name.toLowerCase -> m).toMap
  lazy val marketEAIMap = allFuturesMarkets.flatMap(m => m.eaiQuoteID.map(_ -> m)).toMap

  val allIndexes: List[Index]
  lazy val indexNameMap = allIndexes.map(m => m.name.toLowerCase -> m).toMap
  lazy val indexEAIMap:Map[Int, Index] = allIndexes.flatMap(m => m.eaiQuoteID.map(_ -> m)).toMap
}

object MarketProvider {
  private var impl: Option[MarketLookup] = None

  def registerImpl(i: MarketLookup) {
    impl match {
      case None => impl = Some(i)
      case Some(_) => throw new Exception("Market provider implementation already registered")
    }
  }

  def registerNewImplForTesting(i: Option[MarketLookup]) {
      impl = i
  }

  def provider = {
    impl match {
        // if you get this when running a test change the test to extend TestMarketSpec
      case None => throw new Exception("Market provider implementation not yet registered")
      case Some(i) => i
    }
  }
}
