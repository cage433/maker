package starling.db


import org.springframework.jdbc.datasource.{DataSourceUtils, DataSourceTransactionManager}
import javax.sql.DataSource
import starling.utils.sql._
import starling.dbx.QueryBuilder._
import org.springframework.transaction.support.{TransactionCallback, TransactionTemplate}
import org.springframework.transaction.{TransactionStatus, TransactionDefinition}
import starling.daterange._
import scala.collection.mutable.{ListBuffer, HashMap => MMap}
import scala.collection.JavaConversions._
import scala.collection.JavaConversions
import java.sql.{PreparedStatement, Connection, ResultSet}
import org.springframework.jdbc.core.namedparam.{MapSqlParameterSource, NamedParameterUtils, NamedParameterJdbcTemplate}
import org.springframework.jdbc.core.{PreparedStatementCreatorFactory, PreparedStatementCallback, RowMapper}
import org.springframework.jdbc.core.simple.SimpleJdbcInsert
import org.springframework.jdbc.`object`.BatchSqlUpdate
import starling.quantity.{SpreadQuantity, Percentage, Quantity}
import starling.eai.TreeID
import starling.utils.{CaseInsensitive, Log}
import starling.instrument.utils.StarlingXStream
import starling.dbx._
import starling.props.ConnectionParams
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import xml.{Elem, PrettyPrinter}

trait DBTrait[RSR <: ResultSetRow] extends Log {
  val dataSource: DataSource

  def resultSetFactory: ResultSetRowFactoryTrait[RSR]

  /**
   * Run f in a transaction. A DBWriter instance is passed as a param which allows updating
   * the DB. Any exceptions thrown from f will cause a rollback and then be propagated up the stack
   * as a nested RunTimeException. If no exception happens a commit is done.
   */
  def inTransaction[T](f: DBWriter => T):T = {
    inTransaction(DB.DefaultIsolationLevel)(f)
  }

  /**
   * Same as above with specific isolationLevel
   */

  def inTransaction[T](isolationLevel: Int)(f: DBWriter => T): T = {
    withTransaction(isolationLevel, false) {
      f(createWriter)
    }
  }

  private def withTransaction[T](isolationLevel: Int, readonly: Boolean)(f: => T): T = {
    val tt = new TransactionTemplate(new DataSourceTransactionManager(dataSource))
    tt.setReadOnly(readonly)
    tt.setIsolationLevel(isolationLevel)
    tt.setPropagationBehavior(TransactionDefinition.PROPAGATION_NESTED)
    tt.execute(new TransactionCallback[T] {
      def doInTransaction(status: TransactionStatus) = log.debugWithTime(status.isNewTransaction ? "TXN" | "OLD TXN") {
        try {
          f
        }
        catch {
          // transaction only rolls back on non-checked exceptions which is great for Java but
          // not so good for Scala. This fixes it.
          case th: Throwable => {
            log.error("******************* DB **************** doInTransaction caught throwable", th)
            throw new RuntimeException(th)
          }
        }
      }
    })
  }

  def selectAll(table: String) = {
    queryWithResult((select("*") from table)) {rs => rs.toString}.mkString("\n")
  }

  class NonAtomicInteger() {
    private var counter: Int = 0

    def incrementAndGet = { counter += 1; counter } // Cheaper than AtomicInteger.incrementAndGet ?
    def get = counter
  }

  def query(sql: String, parameters: Map[String, Any] = Map[String, Any]())(f: RSR => Unit): Int = {
    log.info(sql)
    try {
      val counter = new NonAtomicInteger
      withTransaction(DB.DefaultIsolationLevel, true) {
        new NamedParameterJdbcTemplate(dataSource).query(sql, convertTypes(parameters), new RowMapper[Unit]() {
          def mapRow(rs: ResultSet, rowNum: Int) = {
            f(resultSetFactory.create(rs, counter.incrementAndGet)); Unit
          }
        })
        null
      }
      counter.get
    }
    catch {
      case e: RuntimeException if e.getCause != null => throw e.getCause // unwrap exception from transaction call
      case e => throw e
    }
  }

  /**
   * Same as query but results of function f are returned as a List
   */
  def queryWithResult[T](sql: String, parameters: Map[String, Any] = Map[String, Any]())(f: RSR => T): List[T] = {
    log.info(sql + ", " + parameters)

    val counter = new NonAtomicInteger
    withTransaction(DB.DefaultIsolationLevel, true) {
      new NamedParameterJdbcTemplate(dataSource).query(sql, convertTypes(parameters), new RowMapper[T]() {
        def mapRow(rs: ResultSet, rowNum: Int) = f(resultSetFactory.create(rs, counter.incrementAndGet)).asInstanceOf[T]
      })
    }.asInstanceOf[java.util.List[T]].toList
  }

  private def renderer = new SqlRenderer

  def queryWithResult[T](q: Query)(f: RSR => T): List[T] = {
    val sql = renderer.render(q)
    queryWithResult(sql.query, sql.parameters)(f)
  }

  /**
   * Same as query but returns an option that's either None or Some on the first result.
   *
   * Any results returned after the first will be ignored so make sure you call this with a correct ordering.
   */
  def queryWithOneResult[T](sql: String, parameters: Map[String, Any]=Map())(f: RSR => T): Option[T] = {
    (queryWithResult(sql, parameters)(f): @unchecked) match {
      case Nil => None
      case element :: _ => Some(element)
    }                              
  }


  def queryWithOneResult[T](query: Query)(f: RSR => T): Option[T] = {
    val sql = renderer.render(query)
    queryWithOneResult(sql.query, sql.parameters)(f)
  }

  def query(q: Query)(f: RSR => Unit): Int = {
    val sql = renderer.render(q)
    query(sql.query, sql.parameters)(f)
  }

  def metadata = dataSource.getConnection.getMetaData

  def convertTypes(params: Map[String, Any]): java.util.Map[String, AnyRef] = DBConvert.convertTypes(params)

  def lookupTable(table: String, from: String, to: String) = queryWithResult(select(from + ", " + to) from(table)) { rs =>
    rs.getString(from) → rs.getString(to)
  }.toMap

  def lookupTable[K, V](table: String, from: String, to: String, k: String => K, v: (K, String) => V): Map[K, V] =
    lookupTable(table, from, to).mapKeys(k).map(kv => kv._1 → v(kv._1, kv._2))

  lazy val tables: scala.List[TableMeta] = {
    val rs = dataSource.getConnection.getMetaData.getColumns(null, "%", "%", "%")
    val tables = MMap[String, TableMeta]().withDefault(tableName => TableMeta(tableName, Nil))

    while (rs.next) {
      tables.updateValue(rs.getString(3), _ + rs.getString(4))
    }

    tables.values.toList.sortBy(_.name)
  }

  protected def createWriter: DBWriter = new DBWriter(DBTrait.this, dataSource)
}

object DBConvert {
  // The strike column is a string so we can't pass it a double as sql server will convert it (badly).
  // same for initial price
  val Strike = CaseInsensitive("strike")
  val InitialPrice = CaseInsensitive("initialprice")
  val prettyPrinter = new PrettyPrinter(100, 2)

  def convertTypes(params: Map[String, Any]): java.util.Map[String, AnyRef] = {
    var convertedMap = scala.collection.mutable.Map[String, AnyRef]()
    for ((key, value) <- params) {
      val actualValue = value match {
        case Some(s) => s
        case None => null
        case o => o
      }
      actualValue match {
        case d: Day => convertedMap += (key -> d.toSqlDate)
        case y: Year => convertedMap += (key -> y.yearNumber.asInstanceOf[java.lang.Integer])
        case t: Timestamp => convertedMap += (key -> new java.util.Date(t.instant))
        case q: Quantity if (Strike == key || InitialPrice == key) => convertedMap += (key -> q.value.toString, key + "UOM" -> q.uom.toString)
        case q: Quantity => convertedMap += (key -> q.value.asInstanceOf[java.lang.Double], key + "UOM" -> q.uom.toString)
        case s: SpreadQuantity => convertedMap += (key -> s.valueString, key + "UOM" -> s.uom.toString)
        case d: DateRange => convertedMap += (key -> d.toString)
        case x: Elem => convertedMap += (key -> prettyPrinter.format(x))
        case c: java.lang.Character => convertedMap += (key -> c.toString)
        case s: Iterable[_] => convertedMap += (key -> JavaConversions.bufferAsJavaList(ListBuffer() ++= convertValuesForIn(s).toList))
        case d: java.util.Date => convertedMap += (key -> d)
        case b: java.lang.Boolean => convertedMap += (key -> b)
        case n: java.lang.Number => convertedMap += (key -> n)
        case t: TreeID => convertedMap += (key -> t.id.asInstanceOf[java.lang.Integer])
        case p: Percentage => convertedMap += (key -> double2Double(100.0 * p.value))
        case o: PersistAsBlob => convertedMap += (key -> StarlingXStream.write(o.obj))
        case null => convertedMap += (key -> null)
        case _ => convertedMap += (key -> actualValue.toString)
      }
    }
    convertedMap
  }

  private def convertValuesForIn(iterable:Iterable[_]) = {
    iterable.map( v => {
      v match {
        case d:Day => d.toSqlDate
        case _ => v.toString
      }
    })
  }
}

/**
 * Created by DBTrait.inTransaction
 *
 * Don't create this yourself.
 */
class DBWriter protected[db](dbTrait: DBTrait[_ <: ResultSetRow], dataSource: DataSource) {
  private def renderer = new SqlRenderer

  def withIdentityInsert[A](tableName: String)(f: => A): A = {
    try {
      setIdentityInsert(tableName, true)
      f
    } finally {
      setIdentityInsert(tableName, false)
    }
  }

  private def setIdentityInsert(tableName: String, on: Boolean) {
    val connection = DataSourceUtils.getConnection(dataSource)
    val flag = if (on) " ON" else " OFF"
    val configSQL = "SET IDENTITY_INSERT " + tableName + flag

    val statement = connection.createStatement
    try {
      statement.execute(configSQL)
    } finally {
      try {
        statement.close
      }
      catch {
        case _ =>
      }
    }
  }

  /**
   * Abort the current transaction and rollback
   */
  def abort {
    val connection = DataSourceUtils.getConnection(dataSource)
    connection.rollback
  }

  def update(sql: String) = {
    new NamedParameterJdbcTemplate(dataSource).update(sql, null.asInstanceOf[java.util.Map[String, Object]]);
  }

  def updateMany(sqls: String*) = sqls.map(update(_))

  def update(sql: String, params: Map[String, Any]) = {
    new NamedParameterJdbcTemplate(dataSource).update(sql, dbTrait.convertTypes(params));
  }

  def queryForUpdate(q: Query)(f: UpdateableResultSetRow => Unit) {
    val sql = renderer.render(q)
    val parsedSql = NamedParameterUtils.parseSqlStatement(sql.query)
    val paramSource = new MapSqlParameterSource(dbTrait.convertTypes(sql.parameters))
    val sqlToUse = NamedParameterUtils.substituteNamedParameters(parsedSql, paramSource)
    val params = NamedParameterUtils.buildValueArray(parsedSql, paramSource, null)
    val paramTypes = NamedParameterUtils.buildSqlTypeArray(parsedSql, paramSource)
    val pscf = new PreparedStatementCreatorFactory(sqlToUse, paramTypes)
    pscf.setUpdatableResults(true)
    queryForUpdate(connection => pscf.newPreparedStatementCreator(params).createPreparedStatement(connection))(f)
  }


  def queryForUpdate(query: String)(f: UpdateableResultSetRow => Unit) {
    queryForUpdate(connection => connection.prepareStatement(query, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE))(f)
  }

  private def queryForUpdate(statementCreator: (Connection) => PreparedStatement)(f: UpdateableResultSetRow => Unit) {
    var connection: Connection = null
    var rs: ResultSet = null
    try {
      connection = DataSourceUtils.getConnection(dataSource)
      assert(DataSourceUtils.isConnectionTransactional(connection, dataSource), "Connection isn't in a transaction.")
      val statement = statementCreator(connection)
      try {
        try {
          rs = statement.executeQuery()
          val updateableResultSet = new UpdateableResultSetRow(rs)
          while (rs.next) {
            f(updateableResultSet)
          }
        } finally {
          if (rs != null) rs.close
        }
      } finally {
        if (statement != null) statement.close
      }
    } finally {
      if (connection != null) DataSourceUtils.releaseConnection(connection, dataSource)
    }
  }

  /**
   * Update tableName with params given where clause
   */
  def update(tableName: String, params: Map[String, Any], where: Clause) {
    val (whereSQL, map) = renderer.expandClause(where)
    val sql = "update " + tableName + "\n" +
            "   set " + params.map {e => e._1 + " = :" + e._1}.mkString(", ") + "\n" +
            " where " + whereSQL
    new NamedParameterJdbcTemplate(dataSource).update(sql, dbTrait.convertTypes(params ++ map))
  }

  /**
   * batch update
   */
  def update(tableName: String, params: Seq[Map[String, Any]], where: Clause) {
    throw new Exception("Not tested")
    if (params.size > 0) {
      val (whereSQL, map) = renderer.expandClause(where)
      val columns = Set.empty[String] ++ params.flatMap(e => e.keySet.map(s => s.toUpperCase))

      val sql = "update " + tableName + "\n" +
              "   set " + columns.map {e => e + " = :" + e}.mkString(", ") + "\n" +
              " where " + whereSQL
      val query = new BatchSqlUpdate(dataSource, sql)
      query.compile
      for(p <- params) {
        val updates = (columns -- p.keySet).map(c => (c -> null))
        query.updateByNamedParam(dbTrait.convertTypes(p ++ updates))
      }
      query.flush
    }
  }

  /**
   * Inserts into tableName. The keys from params provide the column names.
   */
  def insert(tableName: String, params: Map[String, Any]): Boolean = {
    val convertedMap: java.util.Map[String, AnyRef] = dbTrait.convertTypes(params)
    val query = new SimpleJdbcInsert(dataSource).withTableName(tableName).usingColumns(convertedMap.keysIterator.toList.toArray: _*)
    query.execute(convertedMap)
    true
  }

  /**
   * Batch insert
   */
  def insert(tableName: String, params: Seq[Map[String, Any]]) {
    if (params.size > 0) {
      val arrayOfParams = new Array[java.util.Map[String, AnyRef]](params.size)
      var index = 0
      for (param <- params) {
        arrayOfParams(index) = dbTrait.convertTypes(param)
        index += 1
      }
      val columns = Set.empty[String] ++ arrayOfParams.flatMap(e => e.keySet.map(s => s.toUpperCase))
      val query = new SimpleJdbcInsert(dataSource).withTableName(tableName).
              usingColumns(columns.toArray: _*)
      query.executeBatch(arrayOfParams)
    }
  }

  /**
   * Inserts into tableName. The keys from params provide the column names.
   */
  def insertAndReturnKey(tableName: String, keyColumn: String, params: Map[String, Any], usingColumns:Option[List[String]]=None): Long = {
    var query = new SimpleJdbcInsert(dataSource).withTableName(tableName).usingGeneratedKeyColumns(keyColumn)
    usingColumns.map { c => query = query.usingColumns(c.toArray : _*) }
    val convertedMap = dbTrait.convertTypes(params)
    query.executeAndReturnKey(convertedMap).longValue
  }

  def delete(tableName: String, where: Clause): Unit = delete(tableName, Some(where))

  private def delete(tableName: String, where: Option[Clause]): Unit = {
    val (whereSQL, map) = renderer.renderClause(where)
    val sql = "delete from " + tableName + " " + whereSQL
    new NamedParameterJdbcTemplate(dataSource).update(sql, dbTrait.convertTypes(map))
  }

  // execute a query which returns no results - at time of writing, that's just an update statement
  def queryWithNoResults(sql : String, parameters : Map[String, Any] = Map[String, Any]()) {
    new NamedParameterJdbcTemplate(dataSource).execute(sql, dbTrait.convertTypes(parameters),
      new PreparedStatementCallback[Unit] {
        def doInPreparedStatement(p1: PreparedStatement) = {p1.execute; Unit}
      })
  }

  def queryWithNoResults(modify: Modify) {
    val sql = renderer.render(modify)
    queryWithNoResults(sql.query, sql.parameters)
  }
}

class DB(val dataSource: DataSource) extends DBTrait[ResultSetRow] {
  lazy val factory = new ResultSetRowFactory

  def resultSetFactory = factory
}

object DB {
  import java.sql.Connection._

  def apply(connectionParams: ConnectionParams) = new DB(DataSourceFactory.getDataSource(connectionParams.url, connectionParams.username, connectionParams.password))

  val DefaultIsolationLevel = TRANSACTION_READ_COMMITTED
}

case class TableMeta(name: String, columns: List[String]) {
  def +(column: String) = copy(columns = column :: columns)
}