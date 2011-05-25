package starling.db

import java.sql.Connection
import org.scalatest.matchers.ShouldMatchers
import org.springframework.jdbc.datasource.SingleConnectionDataSource
import org.testng.annotations.{AfterTest, BeforeTest, Test}

import starling.market.Market
import starling.pivot.model.PivotTableModel
import starling.quantity.{Quantity, UOM}
import starling.daterange._
import starling.gui.api._
import starling.marketdata._
import starling.pivot._
import java.lang.String
import starling.richdb.{RichResultSetRowFactory, RichDB}
import collection.immutable.{Nil, Map}
import starling.utils.sql.ConnectionParams
import starling.utils.{StarlingTest, Broadcaster}


class MarketDataStoreTest extends StarlingTest with ShouldMatchers {

  lazy val marketDataStore = new DBMarketDataStore(db, Map(), Broadcaster.Null)

  var db : RichDB = _
  var connection : Connection = _

  @BeforeTest
  def initialise {
    connection = DBTest.getConnection("jdbc:h2:mem:marketDataStoreTest;create=true")
    val ds = new SingleConnectionDataSource(connection, true)
    db = new RichDB(ds, new RichResultSetRowFactory)
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

  @Test
  def testDeletingPricesIsPersistent() {

    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))

    val existingVersion = marketDataStore(MarketDataID(observationPoint, MarketDataSet.Starling, key))
    existingVersion should be === None

    marketDataStore.saveActions(Map(MarketDataSet.Starling -> List(MarketDataUpdate(observationPoint, key, Some(data1), None))))
    val versionAfterDelete = marketDataStore(MarketDataID(observationPoint, MarketDataSet.Starling, key))
    versionAfterDelete should not be === (None)

    val versionInt = versionAfterDelete.get.version
    val versionedData1 = VersionedMarketData(Timestamp.now, versionInt, data1)
    marketDataStore.saveActions(Map(MarketDataSet.Starling -> List(MarketDataUpdate(observationPoint, key, None, Some(versionedData1)))))
    val read2:Option[SpotFXData] = marketDataStore.readLatest(MarketDataSet.Starling, observationPoint, key)
    read2 should equal( None )

    val pfs = PivotFieldsState(
      dataFields=List(SpotFXDataType.rateField.field),
      rowFields=List(SpotFXDataType.currencyField.field)
    )
    val marketDataIdentifier = MarketDataIdentifier(
      MarketDataSelection(Some(PricingGroup.Starling)),
      SpecificMarketDataVersion(marketDataStore.latestPricingGroupVersions(PricingGroup.Starling))
    )

    val pivotData = marketDataStore.pivot(marketDataIdentifier, SpotFXDataType).data(pfs)
    pivotData.data should equal (Nil)

    val priceData = new NormalMarketDataReader(marketDataStore, marketDataIdentifier).readAllPrices(observationPoint)
    priceData should equal (Nil)

    marketDataStore.query(marketDataIdentifier, PriceDataType) should equal (Nil)
  }

  @Test
  def testWritingSinglePriceIsPersistent() {
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, observationPoint, key, data1)
    val read1:SpotFXData = marketDataStore.readLatest(MarketDataSet.Starling, observationPoint, key).get
    read1 should equal( data1 )

    val data2 = SpotFXData(Quantity(2, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, observationPoint, key, data2)
    val read2:SpotFXData = marketDataStore.readLatest(MarketDataSet.Starling, observationPoint, key).get
    read2 should equal( data2 )
  }

  @Test
  def testOverridenPricesAreMerged() {
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = PriceDataKey(Market.LME_LEAD)

    val excelSet = MarketDataSet.excel("Override")

    val blah: Map[DateRange, Double] = Map(Month(2010, 1) -> 50.0, Month(2010, 2) -> 60.0)
    val basePrices = PriceData.fromMap(blah, key.market.priceUOM)
    val overridingPrices = PriceData.fromMap(Map(Month(2010, 2) -> 80.0, Month(2010, 3) -> 70.0), key.market.priceUOM)
    marketDataStore.save(MarketDataSet.Starling, observationPoint, key, basePrices)
    marketDataStore.save(excelSet, observationPoint, key, overridingPrices)
    val selection = MarketDataSelection(Some(PricingGroup.Starling), Some("Override"))
    val marketDataIdentifier = marketDataStore.latestMarketDataIdentifier(selection)
    val read: MarketData = new NormalMarketDataReader(marketDataStore, marketDataIdentifier).read(observationPoint, key)

    val expected = PriceData.create(List(Month(2010, 1) -> 50.0, Month(2010, 2) -> 80.0, Month(2010, 3) -> 70.0), key.market.priceUOM)
    read should be === expected
  }

  @Test
  def testPivotOverObservationTime() {
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val observationPoint2 = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.LMEClose)
    val key = SpotFXDataKey(UOM.EUR)

    val data1 = SpotFXData(Quantity(3, UOM.EUR / UOM.USD))
    val data2 = SpotFXData(Quantity(7, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, observationPoint, key, data1)
    marketDataStore.save(MarketDataSet.Starling, observationPoint2, key, data2)

    val pivot = marketDataStore.pivot(MarketDataIdentifier(
        MarketDataSelection(Some(PricingGroup.Starling)),
        SpecificMarketDataVersion(marketDataStore.latestPricingGroupVersions(PricingGroup.Starling))
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

    check(pfs1, ",\nCurrency,Rate (EUR/USD)\nEUR,3.0000 ")

    val pfs2 = new PivotFieldsState(
      rowFields = List(Field("Currency")),
      columns = ColumnTrees.createFlat(List(Field("Observation Time")), List(Field("Rate")))
    )

    check(pfs2, ",Default (EUR/USD),LME Close (EUR/USD)\nCurrency,Rate,Rate\nEUR,3.0000 ,7.0000 ")
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

}

