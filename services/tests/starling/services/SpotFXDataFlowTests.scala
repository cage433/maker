package starling.services

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import scalaz._
import Scalaz._
import starling.daterange.{Timestamp, Day}
import starling.gui.api._
import starling.databases.{PricingGroupMarketDataEventSource, MarketDataChange, MarketDataProvider}

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

  val lastVersion = 1//SnapshotIDLabel(1, Timestamp.now, MarketDataSelection.Null, SnapshotType.Manual, 1)
  val observationDay = Day(2011, 3, 1)
  val currentVersion = 2
  val newSnapshot = SnapshotIDLabel(currentVersion, Timestamp.now, MarketDataSelection.Null, SnapshotType.Manual, 2)

  val someData = Map(observationDay → Map("Market" → 1.2))
  val differentData = Map(observationDay → Map("Market" → 2.1))
  val someEvents = List(TestMarketDataEvent(List("Market"), observationDay, newSnapshot, isCorrection = false))
  val someCorrections = List(TestMarketDataEvent(List("Market"), observationDay, newSnapshot, isCorrection = true))

  case class TestMarketDataEvent(merkets: List[String], override val observationDay: Day, override val label: SnapshotIDLabel, isCorrection: Boolean)
    extends MarketDataEvent(observationDay, label, isCorrection)

  def pricingGroupMarketDataEventSource() = {
    val dataFlowDataProvider = mock(classOf[MarketDataProvider[Day, String, Double]])
    val observationDays = List(observationDay)

    new PricingGroupMarketDataEventSource {
      type Key = Day
      type MarketType = String
      type CurveType = Double
      type Value = Map[String, Double]

      def marketDataProvider = Some(dataFlowDataProvider)
      val pricingGroup = PricingGroup.Metals

      def marketDataEvent(change: MarketDataChange, key:Day, marketTypes: List[String], snapshot: SnapshotIDLabel) = {
        Some(TestMarketDataEvent(marketTypes, change.observationDay, snapshot, change.isCorrection))
      }

      def events() = changesFor(lastVersion, currentVersion, observationDays).flatMap(_.eventFor(newSnapshot))

      def receivingNoData() = receivingChangingData(Map.empty, Map.empty)
      def receivingUnchangingData(data: Map[Key, Value]) = receivingChangingData(data, data)
      def receivingNewData(newData: Map[Key, Value]) = receivingChangingData(Map.empty, newData)
      def receivingDeletedData(deleted: Map[Key, Value]) = receivingChangingData(deleted, Map.empty)

      def receivingChangingData(oldData: Map[Key, Value], newData: Map[Key, Value]) = {
        when(dataFlowDataProvider.marketDataFor(PricingGroup.Metals, lastVersion, observationDay)) thenReturn oldData
        when(dataFlowDataProvider.marketDataFor(PricingGroup.Metals, currentVersion, observationDay)) thenReturn newData

        this
      }
    }
  }
}