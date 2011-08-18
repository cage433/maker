package starling.db

import org.mockito.Mockito._
import org.scalatest.matchers.ShouldMatchers
import starling.daterange._
import collection.immutable.{Nil, Map}
import starling.utils.StarlingSpec
import starling.utils.ImplicitConversions._
import starling.calendar.Clock
import starling.marketdata._
import starling.pivot.PivotQuantity
import org.scalatest.{WordSpec, BeforeAndAfterAll}
import starling.market.{TestMarketSpec, TestMarketTest, Market}


class MarketDataImporterTests extends StarlingSpec with ShouldMatchers with BeforeAndAfterAll with TestMarketSpec {
  import MarketDataSet._

  override def beforeAll() = Clock.freeze
  override def afterAll() = Clock.thaw

  val observationDay: Day = Day.today
  val observationPoint = observationDay.atTimeOfDay(ObservationTimeOfDay.LMEClose)
  val marketDataSet = LIM
  val marketDataType: MarketDataType = PriceDataType
  val noUpdates = Map(LIM → Nil)
  val zincWithNoPrices = MarketDataEntry(observationPoint, PriceDataKey(Market.LME_ZINC), PriceData(Map()))
  val zincWithPrices = MarketDataEntry(observationPoint, PriceDataKey(Market.LME_ZINC), PriceData(Map(observationDay → PivotQuantity(1))))
  val zincWithChangedPrices = MarketDataEntry(observationPoint, PriceDataKey(Market.LME_ZINC), PriceData(Map(observationDay → PivotQuantity(2))))
  val leadWithPrices = MarketDataEntry(observationPoint, PriceDataKey(Market.LME_LEAD), PriceData(Map(observationDay → PivotQuantity(1))))

  "An importer with no existing data" should {
    "produce no updates" when {
      "no MarketDataSource is available for the MarketDataSet" in {
        importer.withNoExistingData.withNoSource.updates should be === noUpdates
      }

      "the MarketDataSource is empty" in {
        importer.withNoExistingData.sourceWithNoData.updates should be === noUpdates
      }

      "the MarketDataSource has no data on the observation day" in {
        importer.withNoExistingData.sourceWithNoDataOnObservationDay.updates should be === noUpdates
      }

      "the MarketDataSource only has 'empty' MarketDataEntries" in {
        importer.withNoExistingData.sourceWithData(zincWithNoPrices).updates should be === noUpdates
      }
    }

    "produce saves when the MarketDataSource has data" in {
      importer.withNoExistingData.sourceWithData(zincWithPrices).updates should be === savesFor(noData → zincWithPrices)
    }
  }

  "An importer with some existing data" should {
    "produce no updates when the MarketDataSource is empty" in {
      importer.withExistingData(zincWithNoPrices).sourceWithNoData.updates should be === noUpdates
    }

    "produce deletes when the same data is returned by the MarketDataSource, but the data is 'empty" in {
      importer.withExistingData(zincWithNoPrices).sourceWithData(zincWithNoPrices).updates should be === deletesFor(zincWithNoPrices)
    }

    "produce deletes when the MarketDataSource has no data on the observation day" in {
      importer.withExistingData(zincWithNoPrices).sourceWithNoDataOnObservationDay.updates should be === deletesFor(zincWithNoPrices)
    }

    "produce saves when the same data is returned by the MarketDataSource, but the data has changed" in {
      importer.withExistingData(zincWithPrices).sourceWithData(zincWithChangedPrices).updates should be === savesFor(zincWithPrices → zincWithChangedPrices)
    }

//    "produce saves when additional data is returned by the MarketDataSource" in {
//      importer.withExistingData(zincWithPrices).sourceWithData(zincWithPrices, leadWithPrices).updates should be === savesFor(zincWithPrices → zincWithPrices, noData → leadWithPrices)
//    }
//
//    "produce saves when unchanged data is returned by the MarketDataSource" in {
//      importer.withExistingData(zincWithPrices).sourceWithData(zincWithPrices).updates should be === savesFor(zincWithPrices → zincWithPrices)
//    }
  }

  private val noData: MarketDataEntry = null

  private def savesFor(entries: (MarketDataEntry, MarketDataEntry)*) = Map(LIM → entries.map((saveFor _).tupled).toList)
  private def saveFor(oldEntry: MarketDataEntry, newEntry: MarketDataEntry) = MarketDataUpdate(TimedMarketDataKey(observationPoint, newEntry.key), Some(newEntry.data), optVersioned(oldEntry))
  private def deletesFor(entries: MarketDataEntry*) = Map(LIM → entries.map(deleteFor).toList)
  private def deleteFor(entry: MarketDataEntry) = MarketDataUpdate(TimedMarketDataKey(observationPoint, entry.key), None, Some(versioned(entry)))
  private def versioned(entry: MarketDataEntry) = VersionedMarketData(Clock.timestamp, 0, Some(entry.data))
  private def optVersioned(entry: MarketDataEntry) = if (entry == null) None else Some(versioned(entry))

  private def importer() = {
    val (marketDataStore, marketDataSource) = (mock(classOf[MarketDataStore]), new AdaptingMarketDataSource(mock(classOf[MarketDataSource])))

    new MarketDataImporter(marketDataStore) {
      def updates = super.getUpdates(observationDay, LIM)

      def withNoExistingData() = updateThis {
        when(marketDataStore.marketData(observationDay, observationDay, marketDataType, marketDataSet)) thenReturn Nil
      }

      def withExistingData(entries: MarketDataEntry*) = updateThis {
        when(marketDataStore.marketData(observationDay, observationDay, marketDataType, marketDataSet))
          .thenReturn(entries.map(entry => (TimedMarketDataKey(observationPoint, entry.key), versioned(entry))).toList)
      }

      def withNoSource = updateThis {
        when(marketDataStore.sourceFor(marketDataSet)) thenReturn None
      }

      def sourceWithNoData() = withSource.updateThis {
        when(marketDataSource.read(observationDay)) thenReturn noData
      }

      def sourceWithNoDataOnObservationDay() = withSource.updateThis {
        when(marketDataSource.read(observationDay)) thenReturn noDataOnObservationDay
      }

      def sourceWithData(entries: MarketDataEntry*) = withSource.updateThis {
        when(marketDataSource.read(observationDay)) thenReturn Map((observationDay, observationDay, marketDataType) → entries.toList)
      }

      private def withSource = updateThis {
        when(marketDataStore.sourceFor(marketDataSet)) thenReturn Some(marketDataSource)
      }

      private val noData: Map[(Day, Day, MarketDataType), List[MarketDataEntry]] = Map()
      private val noEntries: List[MarketDataEntry] = Nil

      private val noDataOnObservationDay = Map((observationDay, observationDay, marketDataType) → noEntries)
      private def updateThis(action: Any) = { action; this }
    }
  }
}