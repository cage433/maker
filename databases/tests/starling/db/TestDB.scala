package starling.db

import java.lang.String
import starling.richdb.{RichResultSetRowFactory, RichDB}
import javax.sql.DataSource


class TestDB(dataSource: DataSource, factory: RichResultSetRowFactory) extends RichDB(dataSource, factory) {
  override protected def createWriter = new DBWriter(db, dataSource) {
    // The in memory test database does not support IDENTITY_INSERT
    override def withIdentityInsert[A](tableName: String)(f: => A) = f
  }
}