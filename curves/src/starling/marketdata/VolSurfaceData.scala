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
  type keyType = OilVolSurfaceDataKey
  lazy val keys : List[OilVolSurfaceDataKey] = Market.all.filter(_.volatilityID.isDefined).map(OilVolSurfaceDataKey)

  def marketDataKeyFields = Set(marketField.field)
  override def keyFields = Set(marketField.field, periodField.field, deltaField.field)
  override def valueFields = List(volatilityField.field)
  def createKey(row: Row) = OilVolSurfaceDataKey(Market.fromName(row.string(marketField)))
  def createValue(rows: List[Row]) = {
    val builder = new Builder()
    rows.foreach { row => {
      val period = row[DateRange](periodField)
      val vol = row[Percentage](volatilityField)
      row.string(deltaField) match {
        case "ATM" => builder.addAtm(period, vol)
        case delta => builder.add(period, delta.toDouble, vol)
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

  protected def fieldValuesFor(key: OilVolSurfaceDataKey) = Row(marketField.field → key.market.name)

  def rows(key: OilVolSurfaceDataKey, data: OilVolSurfaceData) = {
    (data.periods zipWithIndex).flatMap { case (period, index) => {
      val atmVol = data.atmVols(index)
      (data.skewDeltas zip data.skews).map { case (delta, vols) => Row(
        OilVolSurfaceDataType.marketField.field → key.market.name,
        OilVolSurfaceDataType.periodField.field → period,
        OilVolSurfaceDataType.deltaField.field → delta.toString,
        OilVolSurfaceDataType.volatilityField.field → vols(index))
      } ++ List(Row(
        OilVolSurfaceDataType.marketField.field → key.market.name,
        OilVolSurfaceDataType.periodField.field → period,
        OilVolSurfaceDataType.deltaField.field → "ATM",
        OilVolSurfaceDataType.volatilityField.field → atmVol))
    } }
  }
}

case class OilVolSurfaceDataKey(market: CommodityMarket) extends MarketDataKey {
  type marketDataType = OilVolSurfaceData
  type marketDataDBType = OilVolSurfaceData
  def typeName = OilVolSurfaceDataType.name
  def subTypeKey = market.toString
  def fields = Set(OilVolSurfaceDataType.marketField.field)
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
  def size = periods.size

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