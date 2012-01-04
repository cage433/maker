package starling.lim

import starling.market
import com.lim.mimapi._
import starling.daterange.Day
import market._
import starling.utils.ImplicitConversions._


trait LIMService {
  def getSpotData(limSymbol: LimSymbol, level: Level, startDate: Day, endDate: Day) : Map[Day, Double]
  def getMultipleData(symbols : List[String], startDate : Day, endDate : Day): Map[Day, Array[Double]]
  def query[T](query: LIMConnection => T): T
}

object LIMService {
  def apply(hostname: String, port: Int): LIMService = new LIMServer(hostname, port)

  object Null extends LIMService {
    def getSpotData(limSymbol: LimSymbol, level: Level, startDate: Day, endDate: Day) = Map()
    def getMultipleData(symbols: List[String], startDate: Day, endDate: Day) = Map()
    def query[T](query: (LIMConnection) => T): T = query(NullConnection)

    private object NullConnection extends LIMConnection {
      def close() {}
      def getAllRelationChildren(relation: String, relationType: Set[RelType]) = Nil
      def getPrices(childRelation: String, level: Level, from: Day, to: Day) = Map()
      def getData(query: String) = Map()
    }
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

trait LIMConnection {
  def close()

  def getAllRelChildren(relations: List[LimNode], relationTypes: Set[RelType] = Set(RelType.CATEGORY)): List[String] =
    relations.flatMap(relation => getAllRelationChildren(relation.name, relationTypes)).toList

  def getAllRelationChildren(relation: String, relationTypes: Set[RelType] = Set(RelType.CATEGORY)): List[String]

  def getPrice(childRelation: String, level: Level, day: Day): Option[Double] =
    getPrices(childRelation, level, from = day, to = day).get(day)

  def getPrices(childRelations: Iterable[String], level: Level, from: Day, to: Day): Map[(String, Day), Double] = {
    childRelations.flatMap(childRelation => getPrices(childRelation, level, from, to).mapKeys(day => childRelation → day)).toMap
  }

  def getPrices(childRelation: String, level: Level, from: Day, to: Day): Map[Day, Double]
  def getPrices(childRelation: String, levels: List[Level], from: Day, to: Day): NestedMap[Level, Day, Double] = {
    levels.toMapWithValues(getPrices(childRelation, _, from, to))
  }

  def getData(query: String): Map[Day, Array[Double]]

  def fromLIM(mimDate: MimDateTime): Day = fromLIM(mimDate.getDate)
  def fromLIM(mimDate: MimDate): Day = Day(mimDate.getYear, mimDate.getMonth, mimDate.getDay)
  def toLIM(day:Day) = new MimDate(day.calendar)
}

class LimNode(parent: Object) {
  private val parentNode : Option[LimNode] = parent.safeCast[LimNode]
  def name: String = parentNode.map(_.name + ":").getOrElse("") + fields(parent).find(_.get(parent) == this).get.getName
  def children = fields(this).map(_.get(this)).toList.asInstanceOf[List[LimNode]]
  override def toString = name
  private def fields(owner: AnyRef) = owner.getClass.getDeclaredFields.map(_.update(_.setAccessible(true)))
}