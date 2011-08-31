package starling.services

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import scalaz._
import Scalaz._
import starling.daterange.{Timestamp, Day}
import starling.gui.api._

class PricingGroupMarketDataEventSourceTests extends WordSpec with ShouldMatchers { test =>
  "PriceFixingDataFlow" should {
    "produce no market data events " when {
      "there's no data" in {
        pricingGroupMarketDataEventSource.receivingNoData.events should be === Nil
      }

      "there's unchanging data " in {
        pricingGroupMarketDataEventSource.receivingUnchangingData(someData).events should be === Nil
      }
    }

    "produce some market data events " when {
      "there's new data " in {
        pricingGroupMarketDataEventSource.receivingNewData(someData).events should be === someEvents
      }
    }

    "produce some market data correction events " when {
      "there's changing data" in {
        pricingGroupMarketDataEventSource.receivingChangingData(someData, differentData).events should be === someCorrections
      }
    }
  }

  val lastSnapshot = SnapshotIDLabel(Day.yesterday, 1, Timestamp.now, 1)
  val currentSnapshot = SnapshotIDLabel(lastSnapshot.observationDay.nextDay, 2, Timestamp.now, 2)

  val someData = Map(currentSnapshot.observationDay → Map("Market" → 1.2))
  val differentData = Map(currentSnapshot.observationDay → Map("Market" → 2.1))
  val someEvents = List(TestMarketDataEvent(List("Market"), currentSnapshot.observationDay, currentSnapshot, isCorrection = false))
  val someCorrections = List(TestMarketDataEvent(List("Market"), currentSnapshot.observationDay, currentSnapshot, isCorrection = true))

  case class TestMarketDataEvent(merkets: List[String], override val observationDay: Day, override val label: SnapshotIDLabel, isCorrection: Boolean)
    extends MarketDataEvent(observationDay, label, isCorrection)

  def pricingGroupMarketDataEventSource() = {
    val dataFlowDataProvider = mock(classOf[DataFlowDataProvider[Day, String, Double]])
    val observationDays = List(lastSnapshot.observationDay, currentSnapshot.observationDay)

    new PricingGroupMarketDataEventSource {
      type Key = Day
      type MarketType = String
      type CurveType = Double
      type Value = Map[String, Double]

      def marketDataProvider = dataFlowDataProvider
      val pricingGroup = PricingGroup.Metals

      def marketDataEvent(observationDay : Day, markets: List[String], snapshot: SnapshotIDLabel, isCorrection: Boolean) = {
        TestMarketDataEvent(markets, observationDay, snapshot, isCorrection)
      }

      def events() = eventsFor(some(lastSnapshot), currentSnapshot, observationDays)

      def receivingNoData() = receivingChangingData(Map.empty, Map.empty)
      def receivingUnchangingData(data: Map[Key, Value]) = receivingChangingData(data, data)
      def receivingNewData(newData: Map[Key, Value]) = receivingChangingData(Map.empty, newData)
      def receivingDeletedData(deleted: Map[Key, Value]) = receivingChangingData(deleted, Map.empty)

      def receivingChangingData(oldData: Map[Key, Value], newData: Map[Key, Value]) = {
        when(dataFlowDataProvider.marketDataFor(any[PricingGroup], same(lastSnapshot), any[List[Day]])) thenReturn oldData
        when(dataFlowDataProvider.marketDataFor(any[PricingGroup], same(currentSnapshot), any[List[Day]])) thenReturn newData

        this
      }
    }
  }
}