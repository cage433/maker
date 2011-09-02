package starling.db

import org.springframework.jdbc.datasource.SingleConnectionDataSource
import starling.richdb.{RichResultSetRowFactory, RichDB}
import org.testng.annotations.{Test, AfterTest, BeforeTest}
import org.testng.Assert._
import java.sql.{DriverManager, Connection}
import starling.market.TestMarketSpec
import starling.daterange.Day
import starling.marketdata.{MarketDataTypes, MarketDataType}

/**
 * Created by IntelliJ IDEA.
 * User: davem
 * Date: 08/08/11
 * Time: 10:08
 * To change this template use File | Settings | File Templates.
 */

object NewSchemaMdDBTest {

  private def buildMessage(actual: Any, expected: Any, msg: String) = {
    "%s\n '- Expected: %s\n '----- Was: %s".format(msg, expected, actual)
  }

  private def assertList(actual: List[Any], expected: List[Any], msg: String): Unit = {
    assertEquals(actual.size, expected.size, buildMessage(actual, expected, msg + " size incorrect"))
    assertTrue(actual == expected, buildMessage(actual, expected, msg + " elements incorrect"))
  }

  private def assertMap[A, B](actual: Map[A, B], expected: Map[A, B], msg: String): Unit = {
    assertEquals(actual.size, expected.size, buildMessage(actual, expected, msg + " size incorrect"))
    assertTrue(actual == expected, buildMessage(actual, expected, msg + " elements incorrect"))
  }
}
// TODO uncomment tests then...
// TODO examine the data differences: are they expected?
// - Some may be, as COLLATE has been introduced in the SELECT statements, and the ordering is not considered important.
// - NOTE: The use of COLLATE ties the implementation to MS-SQL Server.
class NewSchemaMdDBTest extends TestMarketSpec {

  import NewSchemaMdDBTest._

  private var db: RichDB = _
  private var connection: Connection = _
  // new SlowMdDB(db), new MarketDataTags(db), marketDataSources, broadcaster
  lazy val newSchemaMdDB: MdDB = new NewSchemaMdDB(db)
  lazy val slowMdDB: MdDB = new SlowMdDB(db)

//  @BeforeTest
  def initialise() = {
    Class.forName("net.sourceforge.jtds.jdbc.Driver")
    connection = DriverManager.getConnection("jdbc:jtds:sqlserver://TTRAFLOCOSQL08.global.trafigura.com/starling_db_8;instance=DB08", "starling", "ng1lr4ts123!Y^%&$")
    val ds = new SingleConnectionDataSource(connection, true)
    db = new TestDB(ds, new RichResultSetRowFactory)
    // initialise the new schema
    newSchemaMdDB.asInstanceOf[NewSchemaMdDB].initialise
  }

//  @AfterTest
  def tearDown() = {
    connection.close()
  }

  // @Test
  def testCheckIntegrity() = {
    // should always be ok
    newSchemaMdDB.checkIntegrity
  }

  // @Test
  def testMarketDataSets() = {
    val slowNames = slowMdDB.marketDataSetNames()
    val newNames = newSchemaMdDB.marketDataSetNames()
    assertList(newNames, slowNames, "MarketDataSet names")
  }

  // @Test
  def testObservationDaysFor() = {
    val mdSetNames = slowMdDB.marketDataSetNames().filterNot(n => n.startsWith(MarketDataSet.excelPrefix))
    for (mdSetName <- mdSetNames) {
      doTestObservationDaysForMarketDataSets(MarketDataSet(mdSetName) :: Nil)
    }
  }

  // @Test
  def testExcelObservationDays() = {
    val mdSetNames = slowMdDB.marketDataSetNames().filter(n => n.startsWith(MarketDataSet.excelPrefix))
    for (mdSetName <- mdSetNames) {
      doTestObservationDaysForMarketDataSets(MarketDataSet(mdSetName) :: Nil)
    }
  }

  // @Test
  def testLatestExcelVersions() = {
    val expected = slowMdDB.latestExcelVersions()
    val actual = newSchemaMdDB.latestExcelVersions()
    assertMap(actual, expected, "Excel versions")
  }

  // @Test
  def testLatestVersionForMarketDataSets() = {
    val expected = slowMdDB.latestVersionForMarketDataSets()
    val actual = newSchemaMdDB.latestVersionForMarketDataSets()
    assertMap(actual, expected, "Latest versions for market data sets")
  }

  // @Test
  def testMaxVersionForMarketDataSetNames() = {
    // test one at a time
    val mdSetNames = slowMdDB.marketDataSetNames().filter(n => n.startsWith(MarketDataSet.excelPrefix))
    for (mdSetName <- mdSetNames) {
      doTestMaxVersionForMarketDataSetNames(mdSetName :: Nil)
    }
    // erm, that's all for now!
  }

  // @Test
  def testMarketDataTypes() = {
    val version = 466000
    val versionCommitId = 111
    val mdSets = MarketDataSet.Crude :: Nil
    val expected = slowMdDB.marketDataTypes(version, mdSets)
    val actual = newSchemaMdDB.marketDataTypes(versionCommitId, mdSets)
    assertList(actual, expected, "Market data types")
  }

  // @Test
  def testLatestMarketData() = {
    // LIKE [DM] to base the query on known data and try more periods/type combinations
    val from = Day.parse("01JAN2011")
    val to = Day.parse("10AUG2011")
    val mdt = MarketDataTypes.fromName("Price")
    val mds = MarketDataSet("LIM")
    doTestLatestMarketData(from, to, mdt, mds)
  }

  private def doTestLatestMarketData(from: Day, to: Day, marketDataType: MarketDataType, marketDataSet: MarketDataSet) = {
    val expected = slowMdDB.latestMarketData(from, to, marketDataType, marketDataSet)
    val actual = newSchemaMdDB.latestMarketData(from, to, marketDataType, marketDataSet)
    assertEquals(actual.size, expected.size, "Incorrect latest market data counts:\nExpected: " + expected + "\n  Actual: " + actual)
    assertTrue(actual == expected, "Incorrect latest market data elements:\nExpected: " + expected + "\n  Actual: " + actual)
  }

  private def doTestObservationDaysForMarketDataSets(mdSets: List[MarketDataSet]) = {
    // concerned only that each element is returned for observation days?
    val expected = newSchemaMdDB.observationDaysFor(mdSets).sorted
    val actual = slowMdDB.observationDaysFor(mdSets).sorted
    assertList(actual, expected, mdSets + " observation days")
  }

  private def doTestMaxVersionForMarketDataSetNames(names: List[String]) = {
    val expected = slowMdDB.maxVersionForMarketDataSetNames(names)
    val actual = newSchemaMdDB.maxVersionForMarketDataSetNames(names)
    assertEquals(actual, expected, "Incorrect max version for market data set names: " + names)
  }
}
