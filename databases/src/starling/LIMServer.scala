package starling

import curves.MissingMarketDataException
import com.lim.mimapi._
import daterange.Day
import market._
import starling.utils.ImplicitConversions._
import starling.utils.ClosureUtil._
import utils.{MathUtil, Log}
import scalaz._
import Scalaz._

class LIMServer(hostname: String, port: Int) {
  private def openConnection =
    new LIMConnection(ConnectionFactory.connect(hostname, port, "Starling"))

  def getSpotData(limSymbol: LimSymbol, level: Level, startDate: Day, endDate: Day) : Map[Day, Double] = {
    val symbol = limSymbol.name
    val limQL = if (level == Level.Mid) {
      "LET\n ATTR HighXA = High of front " + symbol + "\n ATTR LowXA = Low of front " + symbol + "\n ATTR XvarA = (HighXA + LowXA) / 2\n " +
      "SHOW 1: XvarA WHEN Date is from " + startDate.toLIM + " to " + endDate.toLIM
    } else
      "SHOW 1: " + level.name + " of front " + symbol + " WHEN Date is from " + startDate.toLIM + " to " + endDate.toLIM

    query(_.getData(limQL)).mapValues(_(0)).filterNot{
      case (_, p) => p.isNaN || p.isInfinity
    }
  }

  def getMultipleData(symbols : List[String], startDate : Day, endDate : Day) = {
    val limQL = "Show " + symbols.zipWithIndex.map{case (sym, i) => i + ": " + sym}.mkString("\n") + " when date is from " + startDate.toLIM + " to " + endDate.toLIM

    query(_.getData(limQL))
  }

  def query[T](query: LIMConnection => T): T = using(openConnection)(query)
}

class LIMConnection(connection: MimConnection) extends Log {
  private val dataManager = connection.getDataManager
  private val schema = connection.getSchemaManager

  def close() = connection.disconnect

  def getAllRelChildren(relations: LimNode*): List[String] = relations.flatMap(relation => getAllRelChildren(relation.name)).toList
  def getAllRelChildren(relation: String): List[String] = {
    try {
      def recurse(relation: String): List[RelationChildInfo] = {
        val children = schema.getRelChildren(relation).toList
        children.filter(_.`type` == RelType.CATEGORY).flatMap(child => recurse(relation + ":" + child.childName)) :::
        children.filterNot(_.`type` == RelType.CATEGORY)
      }

      recurse(relation).map(_.childName)
    } catch {
      case e:MimException if e.getMessage.contains("Relation does not exist") => List()
    }
  }

  def getPrice(childRelation: String, level: Level, day: Day): Option[Double] =
    getPrices(childRelation, level, from = day, to = day).get(day)

  def getPrices(childRelations: Iterable[String], level: Level, from: Day, to: Day): Map[(String, Day), Double] = {
    childRelations.flatMap(childRelation => getPrices(childRelation, level, from, to).mapKeys(day => childRelation → day)).toMap
  }

  def getPrices(childRelation: String, level: Level, from: Day, to: Day): Map[Day, Double] = {
    // LIM doesn't return enough information to determine which value is for which relation, so only pass one at a time
    val parameters = new GetRecordsParameters(Array(childRelation), Array(level.name)).update { params =>
      params.setFromDate(toLIM(from))
      params.setToDate(toLIM(to))
    }

    try {
      val result = dataManager.getRecords(parameters)

      val dates = (result.getDateTimes ?? Array()).toList.map(fromLIM)
      // Note:
      // We're doing rounding here as LIM is giving back numbers with floating errors. For fixings it's
      // giving back numbers like 123.24999999991232, which is clearly supposed to be 123.2500
      val values = (result.getValues ?? Array()).toList.map(MathUtil.roundToNdp(_, 4))

      dates.zip(values).filterNot(_._2.isNaN).toMap
    } catch {
      case e: MimException => {
        log.debug("Dodgy LIM", e)

        Map()
      }
    }
  }

  def fromLIM(mimDate: MimDateTime): Day = fromLIM(mimDate.getDate)
  def fromLIM(mimDate: MimDate): Day = Day(mimDate.getYear, mimDate.getMonth, mimDate.getDay)
  def toLIM(day:Day) = new MimDate(day.calendar)

  def getData(query: String): Map[Day, Array[Double]] = {
    try {
      val queryResult = dataManager.queryExecute(query)

      var result = Map[Day, Array[Double]]()
      if ((queryResult.reports != null) && (queryResult.reports.length == 1) && (queryResult.reports(0).blocks.length == 1)) {
        val block = queryResult.reports(0).blocks(0)

        val dates = block.getRowDates
        val numRows = block.getNumRows
        val numCols = block.getNumCols
        for (row <- 0 until numRows)  {
          val day = fromLIM(dates(row))
          // see rounding comment above
          val prices = (0 until numCols).toArray.map(block.getValue(row, _)).map(MathUtil.roundToNdp(_, 4))
          result += day -> prices
        }
        result
      } else{
          log.info("Bad LIM query")
          log.info("\tquery " + query)
          throw new MissingMarketDataException("No LIM data for " + query)
      }
    } catch {
      case e: MimException => {
        throw new MissingMarketDataException("LIM Error: No LIM data for " + query, e)
      }
      case e => {
        log.error("Failed to get LIM data, " + query, e)
        throw e
      }
    }
  }

}

object LIMServer {
  def main(args:Array[String]) {
    val server = new LIMServer("lim-london-live", 6400)
//    Log.infoWithTime("") {
//      (1 until 10).foreach { i => {
//        List("GCC", "GCC.A", "HGC", "PAC", "PLC", "SIC", "SIC.A").foreach { code => {
//          val prices = server.readRelation(Day.today.previousWeekday, "COMEX." + code)
//          val observationDays = TreeSet[Day]() ++ prices.distinctBy(_._2)
//          println(code + " " + prices.size + " " + observationDays.size + " " + observationDays.last + " " + observationDays.head)
//        } }
//    } } }
  }

  val TopRelation = new LimNode(this) {
    val Energy = new LimNode(this) {
      val Tankers = new LimNode(this) {
        val BalticFreight = new LimNode(this) {
          val Index_Forward = new LimNode(this)
        }
      }
    }
    val ForeignExchange = new LimNode(this) {
      val Ecb = new LimNode(this)
    }
    val Trafigura = new LimNode(this) {
      val Bloomberg = new LimNode(this) {
        val Currencies = new LimNode(this) {
          val Composite = new LimNode(this)
          val LME = new LimNode(this)
          val Lme = new LimNode(this)
        }
        val Futures = new LimNode(this) {
          val Comex = new LimNode(this)
          val Shfe = new LimNode(this)
        }
        val InterestRates = new LimNode(this) {
          val Libor = new LimNode(this)
          val Liborlike = new LimNode(this)
          val Swaps = new LimNode(this)
        }
        val Metals = new LimNode(this) {
          val Lme = new LimNode(this)
        }
      }
    }
  }
}

class LimNode(parent: Object) {
  private val parentNode : Option[LimNode] = parent.safeCast[LimNode]
  def name: String = parentNode.map(_.name + ":").getOrElse("") + fields(parent).find(_.get(parent) == this).get.getName
  def children = fields(this).map(_.get(this)).toList.asInstanceOf[List[LimNode]]
  override def toString = name
  private def fields(owner: AnyRef) = owner.getClass.getDeclaredFields.map(_.update(_.setAccessible(true)))
}