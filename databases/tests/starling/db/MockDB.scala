package starling.db

import org.springframework.jdbc.datasource.SingleConnectionDataSource
import starling.richdb.{RichResultSetRowFactory, RichDB}
import starling.market.TestMarketTest
import org.scalatest.testng.TestNGSuite

trait MockDB extends TestMarketTest with TestNGSuite {

  /**
   * Creates a fresh db and allows it to be used with access through a RichDB. Then cleans it up later.
   */
  def inFreshDB(name: String, createStrs: List[(String, String)])(f: (RichDB) => Unit) = {
    val url = "jdbc:hsqldb:mem:" + name + ";create=true"
    val connection = DBTest.getConnection(url)
    val ds = new SingleConnectionDataSource(connection, true)

    val db = new RichDB(ds, new RichResultSetRowFactory)

    db.inTransaction {
      writer => {
        createStrs.map(c => writer.update(c._2))
        try {
          f(db)
        } finally {
          createStrs.map(c => writer.update("drop table " + c._1))
        }
      }
    }
    connection.close
  }
}
