package starling.metals.datasources

import starling.utils.StarlingSpec
import org.scalatest.matchers.ShouldMatchers
import starling.lim.{LIMConnection, LIMService}
import starling.db.MarketDataEntry
import starling.utils.ImplicitConversions._
import starling.quantity.UOM
import collection.immutable.{Map, Set, List}
import starling.market.{FuturesExchangeFactory, Level}
import java.lang.String
import starling.marketdata._
import starling.daterange.{StoredFixingPeriod, ObservationTimeOfDay, Day}, Day._


class RefactoredLimSourcesTests extends StarlingSpec with ShouldMatchers {
  import UOM._;
  import LIMService.TopRelation._
  import Trafigura._
  import ObservationTimeOfDay._
  import Level._

  "Replacement same as Original" in {
    val service = LIMService("ttraflonrh221", 6400)
    val start = 1 Jan 2011
    val end = 3 Jan 2012

    service.query { connection => {

      val actual = mapIt(LIBORFixingsSource.marketDataEntriesFrom(connection, start, end).toSet)
//      val expected = mapIt(new MonthlyFuturesFixings(Bloomberg.Futures.Shfe, FuturesExchangeFactory.SHFE).marketDataEntriesFrom(connection, start, end).toSet)
//
//      actual.keySet should be === expected.keySet
//
//      actual.zipMap(expected).foreach { case (marketDataKey, (actualMD, expectedMD)) =>
//        val a: PriceFixingsHistoryData = actualMD.asInstanceOf[PriceFixingsHistoryData]
//        val e = expectedMD.asInstanceOf[PriceFixingsHistoryData]
//
//        val actualDays = a.fixings.keySet.toList.filterCast[(Level, StoredFixingPeriod)].sorted
//        val expectedDays = e.fixings.keySet.toList.filterCast[(Level, StoredFixingPeriod)].sorted
//
//        (expectedDays -- actualDays) should be === Nil
//        (actualDays -- expectedDays) should be === Nil
//
//        (marketDataKey, actualMD) should be === (marketDataKey, expectedMD)
//      }
      3
    } }
  }

  private def mapIt(set: Set[MarketDataEntry]): Map[MarketDataKey, MarketData] =
    set.map(entry => entry.key → entry.data).toMap
}

