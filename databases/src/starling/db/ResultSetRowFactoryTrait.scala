package starling.db

import starling.instrument.utils.StarlingXStream
import starling.daterange._

import scala.collection.JavaConversions._
import com.thoughtworks.xstream.converters.ConversionException
import starling.quantity._
import java.sql.{ResultSetMetaData, ResultSet}
import starling.utils.cache.CacheFactory
import scalaz.Scalaz._

trait ResultSetRowFactoryTrait[RSR <: ResultSetRow] {
  def create(resultSet: ResultSet, count: Int): RSR
}

class ResultSetRowFactory extends ResultSetRowFactoryTrait[ResultSetRow] {
  def create(resultSet: ResultSet, count: Int) = new ResultSetRow(resultSet, count)
}

class UpdateableResultSetRow(resultSet: ResultSet) extends ResultSetRow(resultSet, 0) {
  def update(values:Map[String,Any]) {
    val convertedParameters = DBConvert.convertTypes(values)
    for ((name, value) <- convertedParameters) {
      resultSet.updateObject(name, value)
    }
    resultSet.updateRow
  }
}

object ResultSetRow {
  val xstreamCache = CacheFactory.getCache("XStream")
}

class ResultSetRow(resultSet: ResultSet, val count: Int) {
  def wasNull() = resultSet.wasNull()

  /**
   * Note: does not work on columns of basic types. e.g. int
   */
  def isNull(column: String) = {
    val obj = resultSet.getObject(column)
    obj == null
  }

  def notNull(column: String) = !isNull(column)

  /**
   * True if the column is part of the result set
   */
  def hasColumn(column: String) = {
    val metaData = resultSet.getMetaData
    (1 to metaData.getColumnCount).exists(i => metaData.getColumnName(i).equalsIgnoreCase(column))
  }

  def getInt(column: String) = resultSet.getInt(column)
  def getIntOption(column: String) = notNull(column) option(getInt(column))

  def getString(column: String) = try{
    resultSet.getString(column) match {
      case null => null
      case s => s.trim // SqlSever seems to add spaces for char columns
    }
  } catch {
    case e =>
      println("Result set = " + this)
      throw e
  }

  def getStringOption(column: String) = getString(column) match {
    case null => None
    case s => Some(s)
  }

  def getDouble(column: String) = resultSet.getDouble(column)
  def getDoubleOption(column : String) = notNull(column) option(getDouble(column))

  def getLong(column: String) = resultSet.getLong(column)

  def getBoolean(column: String) = resultSet.getBoolean(column)

  def getQuantity(column: String, uomColumn: String): Quantity = {
    if(isNull(column)) {
      throw new NullPointerException("Couldn't parse null column ('" + column + "')as quantity")
    } else {
      val d = getDouble(column)
      val uom = resultSet.getString(uomColumn) match {
        // if d != 0 then we definitely want UOM SCALAR - not certain
        // we can distinguish when d == 0 whether the UOM should be
        // NULL or SCALAR - may not matter
        case "" => if (d == 0.0) UOM.NULL else UOM.SCALAR
        case null => if (d == 0.0) UOM.NULL else UOM.SCALAR
        case _ => getUOM(uomColumn)
      }
      Quantity(d, uom)
    }
  }

  def getQuantity(column: String): Quantity = getQuantity(column, column + "UOM")
  def getQuantityOrNullQuantity(column: String): Quantity = if(isNull(column)) {
    Quantity.NULL
  } else {
    getQuantity(column, column + "UOM")
  }
  def getStrikeOrNullStrike(column: String): SpreadOrQuantity = {
    if (isNull(column)) {
      SpreadOrQuantity(Left(Quantity.NULL))
    } else {
      if (getString(column).contains("/")) {
        val str = getString(column) + " " + getString(column + "UOM")
        SpreadOrQuantity(Right(SpreadQuantity.parse(str).get))
      } else {
        SpreadOrQuantity(Left(getQuantity(column, column + "UOM")))
      }
    }
  }

  def getUOM(column: String) = {
    val strUOM = resultSet.getString(column)
    assert(!strUOM.trim.isEmpty, "No UOM for " + column)
    UOM.fromString(strUOM)
  }

  def getTimestamp(column: String = "timestamp") = Timestamp(resultSet.getTimestamp(column).getTime)

  def getCharacter(column: String) = getString(column).charAt(0)

  def getDayOption(column:String) = if (isNull(column)) None else Some(getDay(column))

  def getDay(column: String): Day = {
    Day.fromJavaDate(resultSet.getDate(column))
  }

  def getDateRange(name : String, tenor : Option[TenorType] = None) : DateRange = {
    DateRange.parse(getString(name))
  }

  def getMonth(name : String) : Month = Month.parse(getString(name))

  def getDateRange(startDayColumn: String, endDayColumn : String) = DateRange(getDay(startDayColumn), getDay(endDayColumn))

  def getObject[T](column: String) = getObjectOption[T](column).getOrElse(throw new Exception("value for " + column + " is null"))

  def getObjectOption[T](column: String):Option[T] = {
    val text = getString(column)
    if (text == null) {
      None
    } else {
      //2300ms when reading London Derivatives Options and London Derivatives 17Dec2010
      ResultSetRow.xstreamCache.memoize(text, {
        try {
          Some(StarlingXStream.read(text).asInstanceOf[T])
        } catch {
          case e:Exception => {
            println("XStream error");
            e.printStackTrace()
            println(text)
            println

            throw e
          }
        }
      })
    }
  }

  def getPercentage(column: String): Percentage = {
    val valueAsDouble =getDouble(column)
     Percentage(valueAsDouble / 100.0)
  }

  def updateString(column: String, data : String) = {
    resultSet.updateObject(column, data)
  }

  def asMap = {
    val metaData = resultSet.getMetaData
    Map() ++ (for (i <- 1 to metaData.getColumnCount) yield {
      (metaData.getColumnName(i), resultSet.getObject(i))
    })
  }

  override def toString() = {
    val metaData = resultSet.getMetaData
    (for (i <- 1 to metaData.getColumnCount) yield {
      "(" + metaData.getColumnName(i) + " -> " + resultSet.getObject(i) + ") "
    }).mkString(", ")
  }
}

