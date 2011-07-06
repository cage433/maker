package starling.marketdata

import starling.daterange.{Month, TenorType, DateRange, Day}
import starling.quantity.{UOM, Percentage}
import starling.market._
import starling.curves.{MissingMarketDataException}
import collection.SortedMap
import collection.immutable.TreeMap
import starling.pivot._
import collection.mutable.HashMap

class VolSurfaceData

object OilVolSurfaceDataType extends MarketDataType{
  type dataType = OilVolSurfaceData
  val keys : List[OilVolSurfaceDataKey] = Market.all.filter(_.volatilityID.isDefined).map(OilVolSurfaceDataKey)

  override def keyFields = Set(marketField.field, periodField.field, deltaField.field)
  override def valueFields = Set(volatilityField.field)
  def createKey(values: Map[Field, Any]) = OilVolSurfaceDataKey(Market.fromName(values(marketField.field).asInstanceOf[String]))
  def createValue(values: List[Map[Field, Any]]) = {
    val builder = new Builder()
    values.foreach { row => {
      val period = row(periodField.field).asInstanceOf[DateRange]
      val vol = row(volatilityField.field).asInstanceOf[Percentage]
      row(deltaField.field) match {
        case "ATM" => builder.addAtm(period, vol)
        case delta:String => builder.add(period, delta.toDouble, vol)
      }
    }}
    builder.build
  }

  val marketField: FieldDetails = FieldDetails("Market")
  val periodField: FieldDetails = FieldDetails("Period")
  val deltaField: FieldDetails = new FieldDetails("Delta") {
    override def comparator = new Ordering[Any]() {
      def compare(x: Any, y: Any) = y.asInstanceOf[String].compare(x.asInstanceOf[String])
    }
  }
  val volatilityField: FieldDetails = new PercentageLabelFieldDetails("Volatility")

  val fields = List(marketField, periodField, deltaField, volatilityField)

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(volatilityField.field),
    rowFields=List(periodField.field),
    columnFields=List(deltaField.field)
  )
}

case class OilVolSurfaceDataKey(market: CommodityMarket) extends MarketDataKey {
  type marketDataType = OilVolSurfaceData
  type marketDataDBType = OilVolSurfaceData
  def dataType = OilVolSurfaceDataType
  def subTypeKey = market.toString
  override def rows(data : OilVolSurfaceData) = (data.periods zipWithIndex).flatMap { case (period, index) => {
    val atmVol = data.atmVols(index)
    (data.skewDeltas zip data.skews).map { case (delta, vols) => {
      Map(
        OilVolSurfaceDataType.marketField.field -> market.name,
        OilVolSurfaceDataType.periodField.field -> period,
        OilVolSurfaceDataType.deltaField.field -> delta.toString,
        OilVolSurfaceDataType.volatilityField.field -> vols(index))
    }} ++ List(Map(
        OilVolSurfaceDataType.marketField.field -> market.name,
        OilVolSurfaceDataType.periodField.field -> period,
        OilVolSurfaceDataType.deltaField.field -> "ATM",
        OilVolSurfaceDataType.volatilityField.field -> atmVol))
  }}

  def fieldValues = Map(OilVolSurfaceDataType.marketField.field -> market.name)
}



class Builder {
  class MutableSurface(var atm:Option[Percentage], val deltas:HashMap[Double,Percentage])
  val data = new HashMap[DateRange,MutableSurface]()
  def add(period:DateRange, delta:Double, vol:Percentage) {
    val surface = data.getOrElseUpdate(period, new MutableSurface(None, HashMap()))
    surface.deltas(delta) = vol
  }
  def addAtm(period:DateRange, vol:Percentage) {
    val surface = data.getOrElseUpdate(period, new MutableSurface(None, HashMap()))
    surface.atm = Some(vol)
  }
  def build = {
    val periods = data.keySet.toList.sortWith(_ < _)
    val atmVols = periods.map( period => data(period).atm.get )
    val deltas = data.values.flatMap(_.deltas.keySet).toSet.toList.sortWith(_ < _)
    val skews = deltas.map {
      delta => {
        periods.map { period => {
          val surface = data(period)
          surface.deltas.getOrElse(delta, Percentage(0))
        }}.toArray
      }
    }.toArray
    OilVolSurfaceData(periods.toArray, atmVols.toArray, deltas.toArray, skews)
  }
}

/**
 * Vol surfaces in EAI have ATM vols, together with skews (Maps of period -> vol shifts)
 */
case class OilVolSurfaceData(
  periods : Array[DateRange],    // should be months or days only
  atmVols : Array[Percentage],
  skewDeltas : Array[Double],
  skews : Array[Array[Percentage]]
)
  extends MarketData
{
  assert(periods.map(_.tenor).toSet.size <= 1, "Tenors need to be the same for all periods: " + periods.toList)
  assert(atmVols.forall(_ != null), "Null vol: " + atmVols)
  assert(periods.size == atmVols.size, "The number of periods (" + periods.size + ") " +
    "must match the number of atm vols (" + atmVols.size + ")")
  assert(skewDeltas.size == skews.size, "The number of deltas (" + skewDeltas.size + ") " +
    "must match the number of skews (" + skews.size + ")")

  def nonEmpty = periods.nonEmpty

  if (skews.size > 0) assert(skews(0).size == periods.size, "The number of skew columns (" + skews(0).size + ") must match the number of periods (" + periods.size + ")")

  def isDaily = periods.headOption match {
    case Some(d: Day) => true
    case _ => false
  }

  def days = periods.map(_.asInstanceOf[Day])
  def months = periods.map(_.asInstanceOf[Month])

  override def toString = periods.toList + " " + atmVols.toList + " " + skewDeltas.toList + " " + skews.map(_.toList).toList

  override def hashCode = skews.map(_.toList).toList.hashCode

  override def equals(other:Any) = {
    other match {
      case rhs:OilVolSurfaceData => {
        periods.toList == rhs.periods.toList &&
        atmVols.toList == rhs.atmVols.toList &&
        skewDeltas.toList == rhs.skewDeltas.toList &&
        skews.map(_.toList).toList == rhs.skews.map(_.toList).toList
      }
      case _ => false
    }
  }
}




object BradyFXVolSurfaceDataType extends MarketDataType {
  type dataType = BradyFXVolSurfaceData
  val marketField: FieldDetails = FieldDetails("Market")
  val periodField: FieldDetails = FieldDetails("Period")
  val deltaField: FieldDetails = FieldDetails("Delta")
  val volatilityField: FieldDetails = new PercentageLabelFieldDetails("Volatility")

  def createKey(values: Map[Field, Any]) = throw new Exception("Implement if we need to do Metals VAR")
  def createValue(values: List[Map[Field, Any]]) = throw new Exception("Implement if we need to do Metals VAR")
  def keyFields = throw new Exception("Implement if we need to do Metals VAR")
  def valueFields = throw new Exception("Implement if we need to do Metals VAR")
  val fields = List(marketField, periodField, deltaField, volatilityField)

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(volatilityField.field),
    rowFields=List(periodField.field),
    columnFields=List(deltaField.field)
  )
}

case class BradyFXVolSurfaceDataKey(market : FXMarket)  extends MarketDataKey {
  type marketDataType = BradyFXVolSurfaceData
  type marketDataDBType = BradyFXVolSurfaceData
  def subTypeKey = market.name
  def dataType = BradyFXVolSurfaceDataType
  override def rows(data : BradyFXVolSurfaceData) = {
    (data.deltas zip data.vols).flatMap((tuple2) => {
        val (delta, vols) = tuple2
        (vols zip data.forwardDays).map { case (vol, day) =>
        Map(
          BradyFXVolSurfaceDataType.marketField.field -> market.name,
          BradyFXVolSurfaceDataType.periodField.field -> day,
          BradyFXVolSurfaceDataType.deltaField.field -> delta.toString,
          BradyFXVolSurfaceDataType.volatilityField.field -> vol)
        }
    })
  }
  def fieldValues = Map(BradyFXVolSurfaceDataType.marketField.field->market.name)
}

/**
 * Brady FX vols are a map of period -> vol
 */
case class BradyFXVolSurfaceData(
  forwardDays : Array[Day],
  deltas : Array[Double],
  vols : Array[Array[Percentage]] //deltas, then days
)
  extends MarketData {
  override def toString =
    "BradyFXVolSurfaceData: \n"+
    deltas.toList + "\n " + vols.map(_.toList).mkString("\n ") 
}


/// METALS

object BradyMetalVolsDataType extends MarketDataType {
  type dataType = BradyMetalVolsData
  val marketField: FieldDetails = FieldDetails("Market")
  val periodField: FieldDetails = FieldDetails("Period")
  val deltaField: FieldDetails = FieldDetails("Delta")
  val volatilityField: FieldDetails = new PercentageLabelFieldDetails("Volatility")

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(volatilityField.field),
    rowFields=List(periodField.field),
    columnFields=List(deltaField.field)
  )

  val fields = List(
    marketField,
    periodField,
    volatilityField,
    deltaField)

  def keyFields = throw new Exception("Implement if we need to do Metals VAR")
  def valueFields = throw new Exception("Implement if we need to do Metals VAR")
  def createKey(values: Map[Field, Any]) = throw new Exception("Implement if we need to do Metals VAR")
  def createValue(values: List[Map[Field, Any]]) = throw new Exception("Implement if we need to do Metals VAR")
}

case class BradyMetalVolsDataKey(market : CommodityMarket) extends MarketDataKey {
  type marketDataType = BradyMetalVolsData
  type marketDataDBType = BradyMetalVolsData
  def dataType = BradyMetalVolsDataType
  def subTypeKey =  market.name
  override def rows(data : BradyMetalVolsData) = (data.vols).map{case((delta,period),vol) => {
        Map(
          BradyMetalVolsDataType.marketField.field -> market.name,
          BradyMetalVolsDataType.periodField.field -> period,
          BradyMetalVolsDataType.deltaField.field -> delta,
          BradyMetalVolsDataType.volatilityField.field -> vol)

      }}

  def fieldValues = Map(BradyMetalVolsDataType.marketField.field -> market.name)
}

/**
 * Brady metals vols have ATM values only
 */
case class BradyMetalVolsData(vols:SortedMap[(Double, DateRange), Percentage])
  extends MarketData
{
  def days = (Set() ++ vols.map(_._1._2.asInstanceOf[Day])).toList.sortWith(_ < _)
  def months:List[Month] = (Set() ++ vols.map(_._1._2.asInstanceOf[Month])).toList.sortWith(_ < _)
  def volsForPeriod(period:DateRange): Map[Double, Percentage] = Map() ++ vols.filter(t=>period.contains(t._1._2)).map(t=>t._1._1 -> t._2)
  //override def toString = "Vols: " + (periods zip vols).mkString(", ")
}

object BradyMetalVolsData {
  def create(vols:Map[(Double, DateRange), Percentage]) = {
    new BradyMetalVolsData(TreeMap.empty(tupleOrdering) ++ vols)
  }
  implicit object tupleOrdering extends Ordering[(Double,DateRange)]{
    def compare(lhs : (Double,DateRange), rhs : (Double,DateRange)) : Int = {
      if (lhs._2 != rhs._2) lhs._2.compare(rhs._2) else lhs._1.toInt - rhs._1.toInt
    }
  }
}













