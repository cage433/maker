package starling.db

import java.sql.Connection
import org.scalatest.matchers.ShouldMatchers
import org.springframework.jdbc.datasource.SingleConnectionDataSource
import starling.pivot.model.PivotTableModel
import starling.quantity.{Quantity, UOM}
import starling.daterange._
import starling.gui.api._
import starling.marketdata._
import starling.pivot._
import java.lang.String
import starling.richdb.{RichResultSetRowFactory, RichDB}
import collection.immutable.{Nil, Map}
import starling.utils.{StarlingTest, Broadcaster}
import starling.market.{TestMarketTest, Market}
import org.testng.annotations._
import starling.utils.ImplicitConversions._
import starling.props.PropsHelper

class MarketDataStoreTest extends TestMarketTest with ShouldMatchers {
  import MarketDataStore._

  lazy val marketDataStore = DBMarketDataStore(PropsHelper.defaultProps, db, Map())

  var db : RichDB = _
  var connection : Connection = _

  @BeforeTest
  def initialise {
    connection = DBTest.getConnection("jdbc:h2:mem:marketDataStoreTest;create=true")
    val ds = new SingleConnectionDataSource(connection, true)
    db = new TestDB(ds, new RichResultSetRowFactory)
    db.inTransaction{
      writer => {
        writer.update(create_table)
      }
    }
  }

  @AfterTest
  def tearDown() {
    connection.close()
  }

  private def clearMarketData(){
    db.inTransaction{
      writer => {
        writer.update(clear_table)
      }
    }

  }
  @Test(enabled = false)
  def testDeletingPricesIsPersistent() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)
    val timedKey = TimedMarketDataKey(observationPoint, key)
    clearMarketData()

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))

    val existingVersion = marketDataStore.readLatest(MarketDataID(timedKey, MarketDataSet.Starling))
    existingVersion should be === None

    marketDataStore.saveActions(Map(MarketDataSet.Starling -> List(MarketDataUpdate(timedKey, Some(data1), None))))
    val versionAfterDelete = marketDataStore.readLatest(MarketDataID(timedKey, MarketDataSet.Starling))
    versionAfterDelete should not be === (None)

    val versionInt = versionAfterDelete.get.version
    val versionedData1 = VersionedMarketData(versionInt, Some(data1))
    marketDataStore.saveActions(Map(MarketDataSet.Starling -> List(MarketDataUpdate(timedKey, None, Some(versionedData1)))))
    val read2:Option[SpotFXData] = marketDataStore.readLatest(MarketDataSet.Starling, timedKey)
    read2 should equal( None )

    val pfs = PivotFieldsState(
      dataFields=List(SpotFXDataType.rateField.field),
      rowFields=List(SpotFXDataType.currencyField.field)
    )
    val marketDataIdentifier = MarketDataIdentifier(
      MarketDataSelection(Some(PricingGroup.System)),
      marketDataStore.latestPricingGroupVersions(PricingGroup.System)
    )

    val pivotData = marketDataStore.pivot(marketDataIdentifier, SpotFXDataType).data(pfs)
    pivotData.data should equal (Nil)

    val priceData = new NormalMarketDataReader(marketDataStore, marketDataIdentifier).readAllPrices(observationPoint)
    priceData should equal (Nil)

    marketDataStore.query(marketDataIdentifier, PriceDataType) should equal (Nil)
  }

  @Test(enabled = false)
  def testWritingSinglePriceIsPersistent() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)
    val timedKey = TimedMarketDataKey(observationPoint, key)

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, timedKey, data1)
    val read1:SpotFXData = marketDataStore.readLatest(MarketDataSet.Starling, timedKey).get
    read1 should equal( data1 )

    val data2 = SpotFXData(Quantity(2, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, timedKey, data2)
    val read2:SpotFXData = marketDataStore.readLatest(MarketDataSet.Starling, timedKey).get
    read2 should equal( data2 )
  }

  @Test(enabled = false)
  def testOverridenPricesAreMerged() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = PriceDataKey(Market.LME_LEAD)
    val timedKey = TimedMarketDataKey(observationPoint, key)
    val overrridingMDS = MarketDataSet.excel("Override")
    def priceData(prices: Map[Month, Double]) = PriceData.create(prices, key.market.priceUOM)

    val (jan, feb, mar) = (Month(2010, 1), Month(2010, 2), Month(2010, 3))
    val basePrices = Map(jan → 50.0, feb → 60.0)
    val overridingPrices = Map(feb → 80.0, mar → 70.0)

    marketDataStore.save(MarketDataSet.ManualMetals, timedKey, priceData(basePrices))
    marketDataStore.save(overrridingMDS, timedKey, priceData(overridingPrices))

    val reader = NormalMarketDataReader(marketDataStore, MarketDataSelection(Some(PricingGroup.Metals), Some("Override")))
    val marketData = reader.read(timedKey)

    marketData should be === priceData(basePrices ++ overridingPrices)
  }

  @Test(enabled = false)
  def testPivotOverObservationTime() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val observationPoint2 = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.LMEClose)
    val key = SpotFXDataKey(UOM.EUR)
    val timedKey = TimedMarketDataKey(observationPoint, key)
    val timedKey2 = TimedMarketDataKey(observationPoint2, key)

    val data1 = SpotFXData(Quantity(3, UOM.EUR / UOM.USD))
    val data2 = SpotFXData(Quantity(7, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, timedKey, data1)
    marketDataStore.save(MarketDataSet.Starling, timedKey2, data2)

    val pivot = marketDataStore.pivot(MarketDataIdentifier(
        MarketDataSelection(Some(PricingGroup.System)),
        marketDataStore.latestPricingGroupVersions(PricingGroup.System)
      ),
      SpotFXDataType
    )

    def check(pfs:PivotFieldsState, expected:String) {
      val data = PivotTableModel.createPivotData(pivot, new PivotFieldParams(true, Some(pfs)))
      val csv = data.pivotTable.asCSV
      csv should equal( expected )
    }
    val pfs1 = new PivotFieldsState(
      rowFields = List(Field("Currency")),
      columns = ColumnTrees.dataField(Field("Rate")),
      filters = (Field("Observation Time"), SomeSelection(Set(ObservationTimeOfDay.Default.name))) :: Nil
    )

    check(pfs1, "Currency,Rate (EUR per USD)\nEUR,3.0000 ")

    val pfs2 = new PivotFieldsState(
      rowFields = List(Field("Currency")),
      columns = ColumnTrees.createFlat(List(Field("Observation Time")), List(Field("Rate")))
    )

    check(pfs2, ",Default (EUR per USD),LME Close (EUR per USD)\nCurrency,Rate,Rate\nEUR,3.0000 ,7.0000 ")
  }

  @Test def noPricingGroupContainsConflictingPriorityMarketDataSets {
    pricingGroupsDefinitions.filterValues(_.toMultiMapWithKeys(_.priority).valueExists(_.size > 1)) should be === Map.empty
  }

  @Test def thereAreNoOrphanedPricingGroups {
    (PricingGroup.values \\ Desk.pricingGroups.intersect(pricingGroupsDefinitions.keys.toList)) should be === Nil
  }

  @Test def thereAreNoOrphanedMarketDataSets {
    (MarketDataSet.values \\ Desk.pricingGroups.flatMap(pricingGroupsDefinitions).distinct) should be === Nil
  }

  @Test def everyPricingGroupHasASetOfMarketDataSets {
    pricingGroupsDefinitions.keySet should be === PricingGroup.values.toSet
  }

  private val create_table = """
    create table MarketData (
      version int GENERATED By DEFAULT AS IDENTITY (START WITH 1, INCREMENT BY 1),
      childVersion int,
      observationDay datetime,
      observationTime varchar(30),
      marketDataSet varchar(50),
      marketDataType varchar(255),
      marketDataKey varchar(255),
      data varchar(1024),
      timestamp datetime
    )
  """

  private val clear_table = """
    truncate table MarketData
  """
}

