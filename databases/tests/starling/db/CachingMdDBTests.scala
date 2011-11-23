package starling.db

import org.scalatest.matchers.ShouldMatchers
import starling.daterange._
import Day._
import ObservationTimeOfDay._
import starling.marketdata._
import starling.quantity.{UOM, Quantity}
import UOM._
import starling.market.Market
import starling.utils.{StarlingSpec, JMocker}
import org.jmock.Expectations._

class CachingMdDBTests extends StarlingSpec with JMocker {
  import context._; import expectations._

  "caching mddb" should {
    "only query underlying mddb when cache is empty" in {
      val (underlying, caching) = create

      expecting {
        oneOf(underlying).query(withArg(equal(version)), withArg(equal(marketDataSets)), withArg(equal(marketDataType)),
          withArg(equal(None)), withArg(equal(None)), withArg(equal(None))); will(returnValue(allData))
      }

      whenExecuting {
        import org.scalatest.matchers.ShouldMatchers._

        caching.cache.get(cacheKey) should be === None

        caching.query(version, marketDataSets, marketDataType, None, None, None)

        caching.cache.get(cacheKey) should be === Some(allData)

        caching.query(version, marketDataSets, marketDataType, None, None, None)
      }
    }

    "only query for a subset of the data when cache is empty" in {
      val (underlying, caching) = create

      expecting {
        oneOf(underlying).query(withArg(equal(version)), withArg(equal(marketDataSets)), withArg(equal(marketDataType)),
          withArg(equal(observationDays)), withArg(equal(observationTimes)), withArg(equal(marketDataKeys)))
        will(returnValue(someData))
      }

      whenExecuting {
        import org.scalatest.matchers.ShouldMatchers._

        caching.cache.get(cacheKey) should be === None

        caching.query(version, marketDataSets, marketDataType, observationDays, observationTimes, marketDataKeys)
      }
    }
  }

  private val version = 1
  private val marketDataSets = List(MarketDataSet.LimMetals)
  private val marketDataType = MarketDataTypeName("foo")
  private val (observationDay, observationTime) = (23 Nov 2011, LMEClose)
  private val marketDataKey: MarketDataKey = PriceDataKey(Market.LME_ALUMINIUM)
  private val observationDays = Option(Set(Option(observationDay)))
  private val observationTimes = Option(Set(observationTime))
  private val marketDataKeys = Option(Set(marketDataKey))

  private val cacheKey = (version, marketDataSets.toSet, marketDataType)

  val priceData: PriceData = PriceData(Map(observationDay → Quantity(1.0, USD / MT).pq))
  private val someData = List((tmdk(), priceData))
  private val allData: List[(TimedMarketDataKey, MarketData)] = List((tmdk(), priceData), (tmdk(Default), priceData))

  private def tmdk(obsTime: ObservationTimeOfDay = observationTime) = TimedMarketDataKey(observationDay.atTimeOfDay(obsTime), marketDataKey)

  private def create = {
    val underlying = mock[MdDB]

    (underlying, new CachingMdDB(underlying))
  }
}