package starling.db

import starling.utils.StarlingTest

object DBTest {
  new org.apache.derby.jdbc.AutoloadedDriver()
  Class.forName("org.hsqldb.jdbcDriver")
  Class.forName("org.h2.Driver")
  def getConnection(jdbcUrl:String) = {
    this synchronized { //DriverManager.getConnection doesn't appear to be threadsafe
      java.sql.DriverManager.getConnection(jdbcUrl)
    }
  }

}
class DBTest extends StarlingTest {

  def getConnection(jdbcUrl:String) = DBTest.getConnection(jdbcUrl)
}
