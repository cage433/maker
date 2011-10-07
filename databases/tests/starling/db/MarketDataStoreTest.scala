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

  lazy val marketDataStore = {
    val mddb = new NewSchemaMdDB(db, ReferenceDataLookup.Null) {
      override def marketDataSetNames() = {
        db.queryWithResult("SELECT DISTINCT marketDataSet AS mds FROM MarketDataExtendedKey ORDER BY mds", Map()) {
          rs => rs.getString("mds")
        }
      }
    }

    new DBMarketDataStore(mddb, new MarketDataTags(db), Map(), Broadcaster.Null, ReferenceDataLookup.Null)
  }

  var db : RichDB = _
  var connection : Connection = _

  @BeforeTest
  def initialise {
    connection = DBTest.getConnection("jdbc:h2:mem:marketDataStoreTest;create=true")
    val ds = new SingleConnectionDataSource(connection, true)
    db = new TestDB(ds, new RichResultSetRowFactory)
    db.inTransaction{
      writer => {
        writer.updateMany(createMarketDataCommit, createMarketDataExtendedKey, createMarketDataValueKey, createMarketDataValue)
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
        writer.update(clearMarketDataValue)
      }
    }
  }

  @Test
  def testDeletingPricesIsPersistent() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)
    val timedKey = TimedMarketDataKey(observationPoint, key)
    clearMarketData()

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))

    val existingVersion = marketDataStore.readLatest(MarketDataID(timedKey, MarketDataSet.Starling))
    existingVersion should be === None

    marketDataStore.update(Map(MarketDataSet.Starling -> List(MarketDataUpdate(timedKey, Some(data1), None))))
    val versionAfterDelete = marketDataStore.readLatest(MarketDataID(timedKey, MarketDataSet.Starling))
    versionAfterDelete should not be === (None)

    val versionInt = versionAfterDelete.get.version
    val versionedData1 = VersionedMarketData(versionInt, Some(data1))
    marketDataStore.update(Map(MarketDataSet.Starling -> List(MarketDataUpdate(timedKey, None, Some(versionedData1)))))
    val read2 = marketDataStore.readLatest(MarketDataSet.Starling, timedKey).flatMap(_.data)
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

  @Test
  def testWritingSinglePriceIsPersistent() {
    clearMarketData()
    val observationPoint = ObservationPoint(Day(2011, 1, 1), ObservationTimeOfDay.Default)
    val key = SpotFXDataKey(UOM.EUR)
    val timedKey = TimedMarketDataKey(observationPoint, key)

    val data1 = SpotFXData(Quantity(1, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, timedKey, data1)
    val read1 = marketDataStore.readLatest(MarketDataSet.Starling, timedKey).get.data.get
    read1 should equal( data1 )

    val data2 = SpotFXData(Quantity(2, UOM.EUR / UOM.USD))
    marketDataStore.save(MarketDataSet.Starling, timedKey, data2)
    val read2 = marketDataStore.readLatest(MarketDataSet.Starling, timedKey).get.data.get
    read2 should equal( data2 )
  }

  @Test
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

    marketData should not be === (priceData(basePrices))
    marketData should be === priceData(basePrices ++ overridingPrices)
  }

  @Test
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

  private val createMarketDataCommit = """
  create table MarketDataCommit (
    id            int IDENTITY(1,1) NOT NULL
    , version     int
    , timestamp   datetime
    , username    varchar(128)
  )
  """

  private val createMarketDataExtendedKey = """
  create table MarketDataExtendedKey (
    id                int IDENTITY(1,1) NOT NULL
    , marketDataSet   varchar(128)
    , marketDataType  varchar(128)
    , observationTime varchar(60)
    , marketDataKey   text
    )
  """

  private val createMarketDataValueKey = """
  create table MarketDataValueKey (
    id      int IDENTITY(1,1) NOT NULL
    , valueKey text
  )
  """

  private val createMarketDataValue = """
  create table MarketDataValue (
    observationDay  date
    , extendedKey   int
    , valueKey      int
    , value         decimal(19,8)
    , uom           varchar(12)
    , comment varchar(128) NULL
    , commitId      int
  )
  """

  private val clearMarketDataValue = """
    truncate table MarketDataValue
  """
}

