package starling.curves

import starling.marketdata.{MarketDataType, MarketData, MarketDataKey}
import starling.market.{Market, WTI, FuturesMarket}
import cern.colt.matrix.DoubleMatrix2D
import starling.quantity.{UOM, Quantity, Percentage}
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.daterange._
import scala.math._
import collection.immutable.TreeMap
import starling.pivot._
import collection.mutable.HashMap

class SpreadStdDevs

/**
 * Standard deviation "surfaces" for spread options.
 */
case class SpreadStdDevSurfaceData(
  periods : Array[Period],
  atm : Array[Double],
  call : Array[Double],
  put : Array[Double],
  uom : UOM)
  extends MarketData {
  assert(periods.length == atm.length, "Spread periods must correspond to std.dev. data")
  assert(periods.length == call.length, "Spread periods must correspond to std.dev. data")
  assert(periods.length == put.length, "Spread periods must correspond to std.dev. data")

  //hashCode and equals are overridden because arrays uses reference equality
  override def hashCode() = periods.toList.hashCode ^ atm.toList.hashCode
  override def equals(other:Any) = {
    other match {
      case rhs:SpreadStdDevSurfaceData =>
        periods.toList == rhs.periods.toList &&
        atm.toList == rhs.atm.toList &&
        call.toList == rhs.call.toList &&
        put.toList == rhs.put.toList &&
        uom == rhs.uom
      case _ => false
    }
  }
}

class SpreadStdDevSurfaceDataBuilder {

  var uom:Option[UOM] = None
  val atms = new HashMap[Period,Double]()
  val puts = new HashMap[Period,Double]()
  val calls = new HashMap[Period,Double]()

  def addAtm(period: Period, stdDev:Quantity):Unit = store(atms, period, stdDev)
  def addPut(period: Period, stdDev:Quantity):Unit = store(puts, period, stdDev)
  def addCall(period: Period, stdDev:Quantity):Unit = store(calls, period, stdDev)

  private def store(map:HashMap[Period,Double], period: Period, stdDev:Quantity) {
    map(period) = stdDev.value
    uom match {
      case None => uom = Some(stdDev.uom)
      case Some(existing) => if (stdDev.uom != existing)
         throw new Exception("All uoms for std dev must be the same " + existing + " " + stdDev.uom)
    }
  }

  def build = {
    import Spread._

    val periods = (atms.keySet ++ puts.keySet ++ calls.keySet).toList.sortWith(_ < _).toArray
    SpreadStdDevSurfaceData(
      periods,
      periods.map { p => atms.getOrElse(p, 0.0)},
      periods.map { p => calls.getOrElse(p, 0.0)},
      periods.map { p => puts.getOrElse(p, 0.0)},
      uom.get
    )
  }

}

case class SpreadStdDevSurfaceDataKey(market : FuturesMarket)
        extends MarketDataKey {
  type marketDataType = SpreadStdDevSurfaceData
  type marketDataDBType = SpreadStdDevSurfaceData
  def dataType = SpreadStdDevSurfaceDataType
  def subTypeKey = market.toString

  override def rows(data: SpreadStdDevSurfaceData) = {
    val periods = data.periods zipWithIndex
    val dataRows = periods.flatMap {
      case (period, index) => {
        Map("ATM" -> data.atm(index), "Call" -> data.call(index), "Put" -> data.put(index)).map {
          case (label, sd) => {
            val (gap, first, last) = period match {
              case SpreadPeriod(first: Month, last: Month) => {
                val gap = (last - first) match {
                  case 1 => "Month"
                  case 6 => "Half-Year"
                  case 12 => "Year"
                  case n => n + " Month"
                }
                (gap, first, last)
              }
              case DateRangePeriod(dr: Month) => ("None", dr, dr)
            }
            Map(
              SpreadStdDevSurfaceDataType.marketField.field -> market.name,
              SpreadStdDevSurfaceDataType.spreadTypeField.field -> gap,
              SpreadStdDevSurfaceDataType.firstPeriodField.field -> first,
              SpreadStdDevSurfaceDataType.lastPeriodField.field -> last,
              SpreadStdDevSurfaceDataType.periodField.field -> period,
              SpreadStdDevSurfaceDataType.deltaField.field -> label,
              SpreadStdDevSurfaceDataType.stdDevField.field -> Quantity(sd, data.uom))
          }
        }.toList
      }
    }
    dataRows
  }

  def fieldValues = Map(SpreadStdDevSurfaceDataType.marketField.field -> market.name)
}

object SpreadStdDevSurfaceDataType extends MarketDataType {
  val marketField: FieldDetails = FieldDetails("Market")
  val firstPeriodField: FieldDetails = FieldDetails("First Period")
  val lastPeriodField: FieldDetails = FieldDetails("Last Period")
  val periodField: FieldDetails = FieldDetails("Period")
  val spreadTypeField: FieldDetails = FieldDetails("Spread Type")
  val deltaField: FieldDetails = new FieldDetails("Delta") {
    override def comparator = new Ordering[Any]() {
      def compare(x: Any, y: Any) = {
        val order:List[Any] = List("ATM", "Put", "Call")
        order.indexOf(x) - order.indexOf(y)
      }
    }
  }
  val stdDevField: FieldDetails = new QuantityLabelFieldDetails("Standard Deviation")

  def marketDataKeyFelds = Set(marketField.field)
  override def keyFields = Set(marketField.field, firstPeriodField.field, lastPeriodField.field, periodField.field, deltaField.field, spreadTypeField.field)
  override def valueFields = Set(stdDevField.field)
  override def createKey(values: Map[Field, Any]) = SpreadStdDevSurfaceDataKey(Market.futuresMarketFromName(values(marketField.field).asInstanceOf[String]))
  def createValue(values: List[Map[Field, Any]]) = {
    val builder = new SpreadStdDevSurfaceDataBuilder()
    values.foreach { row => {
      val period = row(periodField.field).asInstanceOf[Period]
      val stdDev = row(stdDevField.field).asInstanceOf[Quantity]
      row(deltaField.field) match {
        case "ATM" => builder.addAtm(period, stdDev)
        case "Put" => builder.addPut(period, stdDev)
        case "Call" => builder.addCall(period, stdDev)
      }
    }}
    builder.build
  }

  val fields = List(marketField, firstPeriodField, lastPeriodField, periodField, deltaField, stdDevField, spreadTypeField)

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(stdDevField.field),
    rowFields=List(spreadTypeField.field, firstPeriodField.field, lastPeriodField.field),
    columnFields=List(deltaField.field)
  )

  val keys = List(SpreadStdDevSurfaceDataKey(Market.NYMEX_WTI))
  type dataType = SpreadStdDevSurfaceData
}

case class SpreadAtmStdDevAtomicDatumKey (
    market : FuturesMarket,
    period : Period,
    override val ignoreShiftsIfPermitted : Boolean = false)
  extends AtomicDatumKey(
    SpreadAtmStdDevCurveKey(market),
    period
   ) with VolKey with EnvironmentDifferentiable
{
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv(this)
  }

  def nullValue = market.standardShift * 2.0

  def periodKey = Some(period)

  def calc_dP(env : Environment) = {
    val csoExpiryDay: Day = market.expiryRule.spreadOptionExpiry(period)
    val T = csoExpiryDay.endOfDay.timeSince(env.marketDay)
    val df = env.discount(market.currency, csoExpiryDay)
    Quantity(0.001, market.priceUOM) * (if (T <= 0) 1.0 else sqrt(Pi / (2.0 * T)) / df.checkedValue(UOM.SCALAR))
  }
  def shiftedEnvs(env : Environment, dP: Quantity) : (Environment, Environment) = {
    val upEnv = env.shiftSpreadStdDevs(market, period, dP)
    val downEnv = env.shiftSpreadStdDevs(market, period, -dP)
    (downEnv, upEnv)
  }
  override def clearProperties : AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = this
}

case class SpreadSkewStdDevAtomicDatumKey(market : FuturesMarket, period: Period)
  extends AtomicDatumKey(SpreadSkewStdDevCurveKey(market), period) {
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv(this)
  }

  def nullValue = Map(0.5 -> Percentage(0))
}

case class SpreadAtmStdDevCurveKey(market : FuturesMarket)
  extends NonHistoricalCurveKey[SpreadStdDevSurfaceData] {

  def marketDataKey = SpreadStdDevSurfaceDataKey(market)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: SpreadStdDevSurfaceData) = {
    new SpreadAtmStdDev(marketDayAndTime, market, marketData)
  }

  def underlying = market.name
}

case class SpreadSkewStdDevCurveKey(market : FuturesMarket)
  extends NonHistoricalCurveKey[SpreadStdDevSurfaceData] {

  def marketDataKey = SpreadStdDevSurfaceDataKey(market)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: SpreadStdDevSurfaceData) = {
    new SpreadSkewStdDevs(marketDayAndTime, market, marketData)
  }

  def underlying = market.name
}

case class SpreadAtmStdDev(
        marketDayAndTime: DayAndTime,
        market : FuturesMarket,
        data: SpreadStdDevSurfaceData
        ) extends CurveObject {
  type CurveValuesType = Quantity

  override def apply(point: AnyRef) : Quantity = {
    val index = data.periods.indexOf(point)
    if (index >= 0) {
      Quantity(data.atm(index), data.uom)
    } else {
      throw new MissingMarketDataException("Missing spread ATM standard deviation for " + point + " in " + market)
    }
  }
}

case class SpreadSkewStdDevs(
        marketDayAndTime: DayAndTime,
        market : FuturesMarket,
        data: SpreadStdDevSurfaceData
        ) extends CurveObject {
  type CurveValuesType = DoubleMatrix2D

  override def apply(point: AnyRef) = {
    val index = data.periods.indexOf(point)
    if (index >= 0) {
      val matrix = new DenseDoubleMatrix2D(1, 2)

      matrix.set(0, 0, data.call(index))
      matrix.set(0, 1, data.put(index))
      matrix
    } else {
      throw new MissingMarketDataException("Missing spread standard deviations for " + point + " in " + market)
    }
  }
}

