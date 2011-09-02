package starling.pivot

import org.springframework.jdbc.datasource.SingleConnectionDataSource
import java.sql.DriverManager
import starling.utils._
import cache.CacheFactory
import starling.utils.sql._
import starling.utils.sql.QueryBuilder._
import starling.db._
import starling.quantity.Quantity
import collection.mutable.LinkedHashSet
import collection.immutable.Set
import collection.Seq
import java.util.concurrent.{Executors, Executor}
import starling.instrument.{DeletedInstrument}
import starling.gui.api.UTPIdentifier
import starling.daterange.{Timestamp, DateRange, Day}

/**
 * An implementation of PivotTableDataSource backed by a database
 * You define the columns to expose and a From clause.
 * The from clause can be a single table or a more complex from with joins and sub queries See the SQL dsl code
 * You can also provide a where clause which is always applied to the generated queries.
 *
 * An sql query is generated based on the locations of the pivot fields
 */
abstract class ColumnDefinition(val name:String) {
  def groupByFields:List[String]
  def selectFields:List[String]
  def orderByFields:List[String]
  def read(resultSet:ResultSetRow):Any
  def filterClauses(values:Set[Any]):List[Clause]
  def fieldDetails = new FieldDetails(name)
}
abstract class FieldBasedColumnDefinition(override val name:String, val sqlName:String, val table:String) extends ColumnDefinition(name) {
  val alias = table+"_"+sqlName
  val fullSqlName = addTableName(sqlName)
  def addTableName(_sqlName : String) = if (table=="") { _sqlName } else { table + "." + _sqlName }
  def groupByFields = List(fullSqlName)
  def selectFields = List(fullSqlName + " " + alias)
  def orderByFields = List(fullSqlName)
  def read(resultSet:ResultSetRow):Any
  def filterClauses(values:Set[Any]):List[Clause] = List( fullSqlName in values )
}

object ColumnDefinition {
  def toSql(name:String) = name.replaceAll(" ", "")
}

case class DayColumnDefinition(override val name:String, override val table:String) extends FieldBasedColumnDefinition(name, ColumnDefinition.toSql(name), table) {
  def read(resultSet: ResultSetRow) = { if (resultSet.isNull(alias)) NullableDay(null) else NullableDay(resultSet.getDay(alias)) }
  override def fieldDetails = new FieldDetails(name) {
    override def nullValue() = NullableDay.Null
  }
  override def filterClauses(values:Set[Any]):List[Clause] = {
    val dayValues = values.asInstanceOf[Set[NullableDay]].filterNot(_==NullableDay.Null).map(_.day)
    val hasNullValue = values.contains(NullableDay.Null)
    //as null is mapped to NullableDay.Null so we have to test for null when this is present
    if (dayValues.isEmpty && !hasNullValue) {
      List(FalseClause)
    } else if (dayValues.isEmpty) {
      List(fullSqlName isNull)
    } else if (!hasNullValue) {
      List(fullSqlName in dayValues)
    } else {
      List( (fullSqlName isNull) or (fullSqlName in dayValues) )
    }
  }
}

case class PeriodColumnDefinition(override val name:String, override val table:String) extends FieldBasedColumnDefinition(name, ColumnDefinition.toSql(name), table) {
  def read(resultSet:ResultSetRow) = if (resultSet.isNull(alias)) NullablePeriod.Null else NullablePeriod.parse(resultSet.getString(alias))
  override def fieldDetails = new FieldDetails(name) {
    override def nullValue() = NullablePeriod.Null
  }
  override def filterClauses(values:Set[Any]) = {
    val deliveryValues = values.asInstanceOf[Set[NullablePeriod]].filterNot(_==NullablePeriod.Null).map(_.period)
    val hasNullValue = values.contains(NullablePeriod.Null)
    if (deliveryValues.isEmpty && !hasNullValue) {
      List(FalseClause)
    } else if (deliveryValues.isEmpty) {
      List(fullSqlName isNull)
    } else if (!hasNullValue) {
      List(fullSqlName in deliveryValues)
    } else {
      List( (fullSqlName isNull) or (fullSqlName in deliveryValues) )
    }
  }
}

case class StringColumnDefinition(override val name:String, override val sqlName:String, override val table:String) extends FieldBasedColumnDefinition(name, sqlName, table) {
  def read(resultSet: ResultSetRow) = { val r = resultSet.getString(alias); if (r==null) "" else r }
  override def filterClauses(values:Set[Any]):List[Clause] = {
    //as null is mapped to ""  we have to test for null when "" is present
    val stringValues = values.filterNot(_==null)
    val hasNullValue = values.contains("")
    if (stringValues.isEmpty && !hasNullValue) {
      List(FalseClause)
    } else if (stringValues.isEmpty) {
      List(fullSqlName isNull)
    } else if (!hasNullValue) {
      List(fullSqlName in stringValues)
    } else {
      List( (fullSqlName isNull) or (fullSqlName in stringValues) )
    }
  }
}
case class TimestampColumnDefinition(override val name:String, override val sqlName:String, override val table:String) extends FieldBasedColumnDefinition(name, sqlName, table) {
  def read(resultSet:ResultSetRow) = {
    resultSet.getTimestamp(alias)
  }
}
case class QuantityStringColumnDefinition(override val name:String, override val table:String) extends FieldBasedColumnDefinition(name, ColumnDefinition.toSql(name), table) {
  override def read(resultSet: ResultSetRow) = resultSet.getQuantityOrNullQuantity(alias)
  override def groupByFields = List(fullSqlName, fullSqlName+"UOM")
  override def selectFields = List(fullSqlName + " " + alias, fullSqlName + "UOM " + alias + "UOM" )
  override def fieldDetails = new QuantityLabelFieldDetails(name)
  override def filterClauses(values: Set[Any]) = {
    if (values.isEmpty) {
      List(FalseClause)
    } else {
      val clauses = values.map { v =>
        val q = v.asInstanceOf[Quantity]
        (new starling.utils.sql.Field(fullSqlName) eql q.value) and (new starling.utils.sql.Field(fullSqlName+"UOM") eql q.uom.asString)
      }
      List( (clauses.head /: clauses.tail) { _ or _ } )
    }
  }
}
case class TradeQuantityStringColumnDefinition(override val name:String, override val table:String) extends FieldBasedColumnDefinition(name, ColumnDefinition.toSql(name), table) {
  val tradeIdName = addTableName("tradeID")
  val tradeID_alias = table +"_tradeID"

  override def read(resultSet: ResultSetRow) = {
    val tradeID = resultSet.getString(tradeID_alias)
    val volume =resultSet.getQuantityOrNullQuantity(alias)
    Map[String, Quantity](tradeID -> volume)
  }
  override def groupByFields = List(fullSqlName, fullSqlName+"UOM")
  override def selectFields = List(fullSqlName + " " + alias, fullSqlName + "UOM " + alias + "UOM" )
  override def fieldDetails = new TradeIDGroupingSumPivotQuantityFieldDetails(name)
  override def filterClauses(values: Set[Any]) = {
    // Incorrect - but would only be called if one of the pre-defined layouts
    // had volume as a filter field - which would never happen.
    List(FalseClause)
  }
}
case class StrikeQuantityStringColumnDefinition(override val name:String, override val table:String) extends FieldBasedColumnDefinition(name, ColumnDefinition.toSql(name), table) {
  override def read(resultSet: ResultSetRow) = resultSet.getStrikeOrNullStrike(alias)
  override def groupByFields = List(fullSqlName, fullSqlName+"UOM")
  override def selectFields = List(fullSqlName + " " + alias, fullSqlName + "UOM " + alias + "UOM" )
  override def fieldDetails = new QuantityLabelFieldDetails(name)
  override def filterClauses(values: Set[Any]) = {
    if (values.isEmpty) {
      List(FalseClause)
    } else {
      val clauses = values.map { v =>
        val q = v.asInstanceOf[Quantity]
        (new starling.utils.sql.Field(fullSqlName) eql q.value) and (new starling.utils.sql.Field(fullSqlName+"UOM") eql q.uom.asString)
      }
      List( (clauses.head /: clauses.tail) { _ or _ } )
    }
  }
}
object StringColumnDefinition {
  def apply(name:String, table:String) = new StringColumnDefinition(name, ColumnDefinition.toSql(name), table)
}
class DoubleSumColumnDefinition(override val name:String, override val sqlName:String, override val table:String)
  extends FieldBasedColumnDefinition(name, sqlName, table) {
  override def read(resultSet: ResultSetRow) = resultSet.getDouble(alias)
  override def groupByFields = List()
  override def selectFields = List("sum("+fullSqlName+") " + alias)
  override def fieldDetails = new SumDoubleFieldDetails(name)
}
class IntColumnDefinition(override val name:String, override val sqlName:String, override val table:String) extends FieldBasedColumnDefinition(name, sqlName, table) {
  override def read(resultSet: ResultSetRow) = resultSet.getInt(alias)
}
class UTPIdentifierColumnDefinition(
  override val name:String, val sqlName:String, val utpTable:String,
  tradeTable : String, partitioningTradeColumns : List[String]
 ) extends ColumnDefinition(name) {
  private val tablesAndNames = (utpTable, sqlName) :: (partitioningTradeColumns.map((tradeTable, _)))
  private def alias(tableAndName : (String, String)) = tableAndName._1 + "_" + tableAndName._2
  private def fullSqlName(tableAndName : (String, String)) = {
    tableAndName match {
      case (table, name) if table == "" => name
      case (table, name) => table + "." + name
    }
  }

  def groupByFields = tablesAndNames.map(fullSqlName)
  def selectFields = tablesAndNames.map{tableAndName => fullSqlName(tableAndName) + " " + alias(tableAndName)}
  def orderByFields = groupByFields
  def read(resultSet: ResultSetRow) = UTPIdentifier(
    resultSet.getInt(alias(utpTable, sqlName)),
    partitioningTradeColumns.map{name => name -> resultSet.getString(alias(tradeTable, name))}.toMap
   )
  def filterClauses(values:Set[Any]):List[Clause] = throw new Exception("Shouldn't be shown to user")

}

class QuantitySumColumnDefinition(override val name:String, override val sqlName:String, override val table:String) extends FieldBasedColumnDefinition(name, sqlName, table) {
  override def read(resultSet: ResultSetRow) = PivotQuantity(resultSet.getQuantity(alias))
  override def groupByFields = List(fullSqlName+"UOM")
  override def selectFields = List("sum("+fullSqlName+") " + alias, fullSqlName + "UOM " + alias + "UOM" )
  override def fieldDetails = new SumPivotQuantityFieldDetails(name)
}

object SQLPivotTableDataSource {

  val executor = Executors.newCachedThreadPool()
  val cache = CacheFactory.getCache("SQLPivotTableDataSource", unique = true)

  def create(
          db:DB,
          columnGroups:List[(String,List[ColumnDefinition])],
          theFrom:From,
          builtInClauses:List[Clause],
          initialState:PivotFieldsState,
          drillDownGroups:List[DrillDownInfo],
          useInMemoryDataSourceOnly : Boolean,
          portfolioTimestamp:Timestamp
          ):PivotTableDataSource = {
    val initialStateX = initialState
    val cols = columnGroups.flatMap(_._2.map(_.name))
    val key = (cols, theFrom, builtInClauses, portfolioTimestamp)
    if (useInMemoryDataSourceOnly)
      cache.memoize(key,
        new InMemorySQLPivotTableDataSource(
          db, columnGroups, theFrom, builtInClauses, initialState, drillDownGroups)
      )
    else {

      cache.getIfComplete[(List[String], From, List[Clause], Timestamp), PivotTableDataSource](key) match {
        case Some(pivot) => pivot
        case None =>
          {
            executor.execute(new Runnable() {
              def run() {
                Thread.sleep(1000) //wait a bit so that the on the fly query runs faster
                cache.memoize(key, new InMemorySQLPivotTableDataSource(
                  db, columnGroups, theFrom, builtInClauses, initialState, drillDownGroups)
                  )
              }
            })
            new OnTheFlySQLPivotTableDataSource(db, columnGroups, theFrom, builtInClauses, initialState, drillDownGroups)
          }
      }
    }
  }
}

class InMemorySQLPivotTableDataSource(
        db:DB,
        columnGroups:List[(String,List[ColumnDefinition])],
        theFrom:From,
        builtInClauses:List[Clause],
        override val initialState:PivotFieldsState,
        override val drillDownGroups:List[DrillDownInfo]) extends UnfilteredPivotTableDataSource {

  val fieldDetailsGroups = columnGroups.map{ case(name, cols) => FieldDetailsGroup(name, cols.map(_.fieldDetails))}
  val columns = columnGroups.flatMap(_._2).filterNot(_.name.equalsIgnoreCase("Costs"))

  val allRows = starling.utils.Log.infoWithTime("Reading all rows") {
    import starling.utils.sql.QueryBuilder._
    val selectString = columns.flatMap(_.selectFields).mkString(", ")
    val groupBys = columns.flatMap(_.groupByFields)
    val query = (select (selectString) from theFrom where builtInClauses groupBy groupBys)
    val columnAndField = columns.map(column => (column, column.fieldDetails.field))
    db.queryWithResult(query) {
      rs => Map() ++ columnAndField.map{case(column,field) => field -> column.read(rs)}
    }
  }

  println("Rows " + allRows.size)

  def unfilteredData(pfs : PivotFieldsState) = allRows
}
class OnTheFlySQLPivotTableDataSource(
        db:DB,
        columnGroups:List[(String,List[ColumnDefinition])],
        theFrom:From,
        builtInClauses:List[Clause],
        override val initialState:PivotFieldsState,
        override val drillDownGroups:List[DrillDownInfo]
        ) extends PivotTableDataSource {

  val fieldDetailsGroups = columnGroups.map{ case(name, cols) => FieldDetailsGroup(name, cols.map(_.fieldDetails))}
  val columns = columnGroups.flatMap(_._2)

  private val columnDefinitions = Map() ++ columns.map { column => column.fieldDetails.field->column }

  private def fieldsToDatabaseFields(fields:Seq[Field]) = fields.map(f=>columnDefinitions.getOrElse(f, throw new Exception(f + " not found in " + columnDefinitions.keySet))).toList

  def data(pfs : PivotFieldsState): PivotResult = {

    val dataAreaFields = fieldsToDatabaseFields(pfs.columns.measureFields)
    val rowAreaFields = fieldsToDatabaseFields(pfs.rowFields)
    val columnAreaFields = fieldsToDatabaseFields(pfs.columns.columnFields)
    val filterAreaFields = pfs.filtersInTheFilterArea.map(entry=>columnDefinitions(entry._1) -> entry._2).toList

    val showFields = rowAreaFields ::: columnAreaFields ::: dataAreaFields

    //For now only the filter area filters are applied using the where clause.
    //Row and column filters are applied in memory
    //This is because calculating the possible values can be slow
    //(as it requires a 'select distinct ...' for each field
    //The OnTheFlySQLPivotTableDataSource is only used for the first trade page
    //which does not have filters in the row and column areas.
    val data =
      if (showFields.isEmpty) {
        List()
      } else {
        val query = buildQuery(dataAreaFields ::: rowAreaFields ::: columnAreaFields, filterAreaFields)
        db.queryWithResult(query) {
          rs=> {
            Map() ++ (showFields.map(f=>f.fieldDetails.field->f.read(rs)))
          }
        }
      }
    val filterAreaPossibleValues = {
      Map() ++ (for ((field,selection) <- filterAreaFields) yield {
        val filtersUpTo = filterAreaFields.slice(0, filterAreaFields.map(_._1).indexOf(field))
        Field(field.name)->readPossibleValues(field, filtersUpTo)
      })
    }

    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(
      fieldDetails,
      data,
      pfs.removeAll(pfs.filterAreaFields.toSet)
    )
    PivotResult(result.data, result.possibleValues ++ filterAreaPossibleValues)
  }

  private def readPossibleValues(field:ColumnDefinition, filters:Seq[(ColumnDefinition,Selection)]):List[Any] = {
    import starling.utils.sql.QueryBuilder._
    var query =
      (select ("distinct " + field.selectFields.mkString(", "))
       from theFrom
       where buildClauses(filters))
    if (!field.orderByFields.isEmpty) {
       query = query orderBy (field.orderByFields.mkString(", ") desc)
    }
    val values = db.queryWithResult ( query ) { rs => field.read(rs) }
    val set = new LinkedHashSet[Any]()
    set ++= values
    set.toList
  }

  private def buildQuery(selectFields:List[ColumnDefinition], where:Seq[(ColumnDefinition,Selection)]) = {
    import starling.utils.sql.QueryBuilder._
    val selectString = selectFields.flatMap(_.selectFields).mkString(", ")
    val groupByFields = selectFields.flatMap(_.groupByFields)
    val clauses = buildClauses(where)
    (select (selectString)) from theFrom where clauses groupBy groupByFields
  }

  private def buildClauses(filter:Seq[(ColumnDefinition,Selection)]):List[Clause] = {
    import starling.utils.sql.QueryBuilder._
    builtInClauses ::: filter.flatMap( t => {
      t._2 match {
        case AllSelection => List()
        case SomeSelection(values) => if (values.isEmpty) List(FalseClause) else t._1.filterClauses(values)
      }
    }).toList
  }

}