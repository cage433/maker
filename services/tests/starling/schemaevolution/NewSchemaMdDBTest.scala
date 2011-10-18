package starling.schemaevolution

import starling.richdb.RichDB
import org.testng.annotations.{Test, AfterTest, BeforeTest}
import org.testng.Assert._
import java.sql.Connection
import starling.utils.ImplicitConversions._
import starling.props.PropsHelper
import starling.services.StarlingInit
import org.scalatest.testng.TestNGSuite
import starling.db._
import collection.immutable.Map
import starling.marketdata._
import org.scalatest.prop.Checkers
import starling.utils.{Log, MemoizedFunction}
import org.scalacheck.{Prop, Gen, Arbitrary}
import Prop._
import starling.daterange.{ObservationTimeOfDay, Day}
import starling.calendar.HolidayTablesFactory


object NewSchemaMdDBTestUtil {
  lazy val (mddb, slow) = {
    HolidayTablesFactory.registerNewHolidayTablesImplForTesting(None)
    val init = new StarlingInit(PropsHelper.defaultProps)

    (new NewSchemaMdDB(init.starlingRichDB, init.dataTypes), new SlowMdDB(init.starlingRichDB))
  }


  def buildMessage(fast: Any, slow: Any, msg: String) = {
    "%s\n '- Slow: %s\n '----- Fast: %s".format(msg, slow, fast)
  }

  def assertList(fast: List[Any], slow: List[Any], msg: String): Unit = {
    assertEquals(fast.size, slow.size, buildMessage(fast, slow, msg + " size incorrect"))
    assertTrue(fast == slow, buildMessage(fast, slow, msg + " elements incorrect"))
  }

  def assertSet(fast: Set[_], slow: Set[_], msg: String): Unit = {
    assertEquals(fast.size, slow.size, buildMessage(fast, slow, msg + " size incorrect"))
    assertTrue(fast == slow, buildMessage(fast, slow, msg + " elements incorrect"))
  }

  def assertMap[A, B](fast: Map[A, B], slow: Map[A, B], msg: String): Unit = {
    val missingFast = fast.keySet -- slow.keySet
    val missingSlow = slow.keySet -- fast.keySet

    assertTrue(missingFast.isEmpty, "missing fast: " + missingFast)
    assertTrue(missingSlow.isEmpty, "missing slow: " + missingSlow)

    val difference = fast.zipMap(slow).filterValues(pair => pair._1 != pair._2)

    assertTrue(difference.isEmpty, "value difference: " + difference)

    assertTrue(fast == slow, buildMessage(fast, slow, msg + " elements incorrect"))
  }
}

// TODO uncomment tests then...
// TODO examine the data differences: are they expected?
// - Some may be, as COLLATE has been introduced in the SELECT statements, and the ordering is not considered important.
// - NOTE: The use of COLLATE ties the implementation to MS-SQL Server.
object NewSchemaMdDBTest extends Checkers with Log {
  def main(args: Array[String]) {
//    testCheckIntegrity
//    testMarketDataSets
    testObservationDaysByMarketDataSet
//    testLatestExcelVersions
//    testLatestVersionForMarketDataSets
//    testMaxVersionForMarketDataSetNames
//    testMarketDataTypes
//    testLatestMarketData
//    testQuery
//    testQueryForObservationDayAndMarketDataKeys
//    testReadLatest
  }

  import NewSchemaMdDBTestUtil._

  private var db: RichDB = _
  private var connection: Connection = _
  // new SlowMdDB(db), new MarketDataTags(db), marketDataSources, broadcaster
  lazy val dataTypes = new MarketDataTypes(ReferenceDataLookup.Null)
  lazy val newSchemaMdDB = new NewSchemaMdDB(db, dataTypes)
  lazy val slowMdDB = new SlowMdDB(db)

//  @BeforeTest
  def initialise() = try {
    val init = new StarlingInit(PropsHelper.defaultProps)
//    Class.forName("net.sourceforge.jtds.jdbc.Driver")
//    connection = DriverManager.getConnection("jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_StacyA1;instance=DB08", "starling", "ng1lr4ts123!Y^%&$")
//    val ds = new SingleConnectionDataSource(connection, true)
    db = init.starlingRichDB
//      new TestDB(ds, new RichResultSetRowFactory)
  } catch {
    case e => println("initialisation died: "); e.printStackTrace; throw e
  }

//  @AfterTest
  def tearDown() = {
    val b = true

//    connection.close()
  }

  lazy val fastNames = newSchemaMdDB.marketDataSetNames()

//  @Test
  def testCheckIntegrity() = {
    // should always be ok
    newSchemaMdDB.checkIntegrity
  }

//  @Test
  def testMarketDataSets() = {
    val slow = slowMdDB.marketDataSetNames()
    val fast = newSchemaMdDB.marketDataSetNames()

    assertList(fast, slow, "MarketDataSet names")
  }

//  @Test
  def testObservationDaysByMarketDataSet {
    val slow = slowMdDB.observationDaysByMarketDataSet
    val fast = newSchemaMdDB.observationDaysByMarketDataSet

    val missingFast = slow.zipMap(fast).mapValues(pair => pair._1 -- pair._2).filterValuesNot(_.isEmpty)
    val missingSlow = fast.zipMap(slow).mapValues(pair => pair._1 -- pair._2).filterValuesNot(_.isEmpty)

    assertTrue(missingFast.isEmpty, "items missing in fast: " + missingFast)
    assertTrue(missingSlow.isEmpty, "items missing in slow: " + missingSlow)

    assertMap(fast, slow, "observation days by market data set")
  }

//  @Test
//  def testObservationDaysFor() = {
//    implicit def arbitraryMarketDataSet : Arbitrary[MarketDataSet] = Arbitrary {
//      Gen.oneOf(fastNames.map(MarketDataSet.fromName(_)))
//    }
//
//    check((marketDataSets : List[MarketDataSet]) => {
//      println("checking observationDaysFor: " + marketDataSets.mkString(", "))
//
//      newSchemaMdDB.observationDaysFor(marketDataSets) == slowMdDB.observationDaysFor(marketDataSets)
//    })
//
//    val mdSetNames = newSchemaMdDB.marketDataSetNames().filterNot(n => n.startsWith(MarketDataSet.excelPrefix))
//    for (mdSetName <- mdSetNames) {
//      doTestObservationDaysForMarketDataSets(MarketDataSet.fromName(mdSetName) :: Nil)
//    }
//  }

//  @Test
//  def testExcelObservationDays() = {
//    val slow = slowMdDB.excelObservationDays().toMap.filterKeys(fastNames.contains)
//    val fast = newSchemaMdDB.excelObservationDays().toMap.filterKeys(fastNames.contains)
//
//    assertMap(fast, slow, "Excel Observation Days")
//  }

//  @Test
  def testLatestExcelVersions() = {
    val slow = slowMdDB.latestExcelVersions().filterKeys(fastNames.contains)
    val fast = newSchemaMdDB.latestExcelVersions().filterKeys(fastNames.contains)

    assertMap(fast, slow, "Excel versions")
  }

//  @Test
  def testLatestVersionForMarketDataSets() = {
    val slow = slowMdDB.latestVersionForMarketDataSets().filterKeys(fastNames.contains)
    val fast = newSchemaMdDB.latestVersionForMarketDataSets().filterKeys(fastNames.contains)

    assertMap(fast, slow, "Latest versions for market data sets")
  }

//  @Test(enabled = false) // I don't see the point of this test, commitId does not == version
  def testMaxVersionForMarketDataSetNames() = {
    // test one at a time
    val mdSetNames = newSchemaMdDB.marketDataSetNames().filter(n => n.startsWith(MarketDataSet.excelPrefix))
    for (mdSetName <- mdSetNames) {
      doTestMaxVersionForMarketDataSetNames(mdSetName :: Nil)
    }
    // erm, that's all for now!
  }

//  @Test
  def testMarketDataTypes() = {
    val version = slowMdDB.latestVersionForAllMarketDataSets().values.max
    val versionCommitId = newSchemaMdDB.latestVersionForAllMarketDataSets().values.max
    val mdSets = newSchemaMdDB.marketDataSetNames().map(MarketDataSet.fromName(_))
//      MarketDataSet.Crude :: Nil
    val slow = slowMdDB.marketDataTypes(version, mdSets)
    val fast = newSchemaMdDB.marketDataTypes(versionCommitId, mdSets)

    assertSet(fast, slow, "Market data types")
  }

//  @Test
  def testLatestMarketData() = {
    // LIKE [DM] to base the query on known data and try more periods/type combinations
    val from = Day.parse("01JAN2011")
    val to = Day.parse("10AUG2011")
    val mdt = PriceDataType
    val mdSets = newSchemaMdDB.marketDataSetNames().map(MarketDataSet.fromName(_))

    mdSets.foreach(mds => doTestLatestMarketData(from, to, mdt, mds))
  }

  private lazy val versionCommits = db.queryWithResult("SELECT * from MarketDataTag where commitId is not null") { rs =>
    (rs.getInt("version"), rs.getInt("commitId"))
  }

  private val extendedKeyIdsFor = MemoizedFunction((commitId: Int) => db.queryWithResult(
    "SELECT DISTINCT extendedKey FROM MarketDataValue WHERE commitId = " + commitId) { rs => rs.getInt("extendedKey") } )

  def observationDaysFor(commitId: Int): List[Option[Day]] = db.queryWithResult(
    "SELECT DISTINCT observationDay FROM MarketDataValue WHERE commitId = " + commitId) { rs => rs.getDayOption("observationDay") }

  def extendedKeysFor(commitId: Int): List[MarketDataExtendedKey] =
    extendedKeyIdsFor(commitId).flatMap(newSchemaMdDB.extendedKeys.get(_))

  def marketDataSetsFor(commitId: Int): List[MarketDataSet] = extendedKeysFor(commitId).map(_.marketDataSet).distinct
  def marketDataKeysFor(commitId: Int): List[MarketDataKey] = extendedKeysFor(commitId).map(_.marketDataKey).distinct
  def observationTimesFor(commitId: Int): List[ObservationTimeOfDay] = extendedKeysFor(commitId).map(_.time).distinct
  def marketDataTypeFor(commitId: Int): Option[MarketDataType] = extendedKeysFor(commitId).map(_.marketDataType).distinct
    .require(_.size <= 1, "More than one MarketDataType for commitId: " + commitId).headOption

  case class QueryParams(version: Int, commitId: Int, marketDataType: MarketDataType, marketDataSets: List[MarketDataSet],
    marketDataKeys: Option[Set[MarketDataKey]], observationDays: Option[Set[Option[Day]]],
    observationTimes: Option[Set[ObservationTimeOfDay]]) {
  }

  implicit def arbitraryQueryParams: Arbitrary[QueryParams] = Arbitrary {
    for (
      (version, commitId) <- Gen.oneOf(versionCommits);
      marketDataSets <- Gen.someOf(marketDataSetsFor(commitId));
      marketDataType <- Gen.oneOf(marketDataTypeFor(commitId).toList);
      marketDataKeys <- Gen.someOf(marketDataKeysFor(commitId));
      observationDays <- Gen.someOf(observationDaysFor(commitId));
      observationTimes <- Gen.someOf(observationTimesFor(commitId))
    )
      yield QueryParams(version, commitId, marketDataType, marketDataSets.toList, Some(marketDataKeys.toSet),
        Some(observationDays.toSet), Some(observationTimes.toSet))
  }

  implicit def arbitraryMarketDataID: Arbitrary[MarketDataID] = Arbitrary {
    for (
      (version, commitId) <- Gen.oneOf(versionCommits);
      marketDataSet <- Gen.oneOf(marketDataSetsFor(commitId));
      observationDay <- Gen.oneOf(observationDaysFor(commitId));
      marketDataKey <- Gen.oneOf(marketDataKeysFor(commitId));
      observationTime <- Gen.oneOf(observationTimesFor(commitId));
      observationPoint <- Gen.oneOf(observationDay.map(_.atTimeOfDay(observationTime)).toList)
    )
      yield MarketDataID(observationPoint, marketDataSet, marketDataKey, dataTypes)
  }

//  @Test
  def testQuery = check((params: QueryParams) => log.infoWithTime(params.toString) {
    val fast = newSchemaMdDB.query(params.commitId, params.marketDataSets, params.marketDataType.name, params.observationDays,
      params.observationTimes, params.marketDataKeys)

    val slow = slowMdDB.query(params.version, params.marketDataSets, params.marketDataType.name, params.observationDays,
      params.observationTimes, params.marketDataKeys)

    (fast == slow) :| ("fast: %s\n!=\nslow: %s" % (fast, slow))
  })

//  @Test
  def testQueryForObservationDayAndMarketDataKeys = check((qp: QueryParams) => {
    val fast = newSchemaMdDB.queryForObservationDayAndMarketDataKeys(qp.commitId, qp.marketDataSets, qp.marketDataType.name)
    val slow = slowMdDB.queryForObservationDayAndMarketDataKeys(qp.version, qp.marketDataSets, qp.marketDataType.name)

    (fast == slow) :| ("fast: %s\n!=\nslow: %s" % (fast, slow))
  })

//  @Test
  def testReadLatest = check((marketDataID: MarketDataID) => {
    val fast = newSchemaMdDB.readLatest(marketDataID)
    val slow = slowMdDB.readLatest(marketDataID)

    (fast == slow) :| ("fast: %s\n!=\nslow: %s" % (fast, slow))
  })

  private def doTestLatestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet) = {
    val fast: Map[TimedMarketDataKey, Option[MarketData]] =
      newSchemaMdDB.latestMarketData(from, to, marketDataType.name, marketDataSet).mapValuesEagerly(_.data)
    val slow: Map[TimedMarketDataKey, Option[MarketData]] =
      slowMdDB.latestMarketData(from, to, marketDataType.name, marketDataSet).mapValuesEagerly(_.data).filterValues(data => {
        data.map(_.size).getOrElse(1) > 0 // retain None & Some containing non-empty market data
      })

    assertMap(fast, slow, "Latest Market Data")

//    assertEquals(actual.size, expected.size, "Incorrect latest market data counts:\nExpected: " + expected + "\n  Actual: " + actual)
//    assertTrue(actual == expected, "Incorrect latest market data elements:\nExpected: " + expected + "\n  Actual: " + actual)
  }

//  private def doTestObservationDaysForMarketDataSets(mdSets: List[MarketDataSet]) = {
//    // concerned only that each element is returned for observation days?
//    val fast = newSchemaMdDB.observationDaysFor(mdSets)
//    val slow = slowMdDB.observationDaysFor(mdSets)
//
//    assertSet(fast, slow, mdSets + " observation days")
//  }

  private def doTestMaxVersionForMarketDataSetNames(names: List[String]) = {
    val slow = slowMdDB.maxVersionForMarketDataSetNames(names)
    val fast = newSchemaMdDB.maxVersionForMarketDataSetNames(names)

    assertEquals(fast, slow, "Incorrect max version for market data set names: " + names)
  }
}
