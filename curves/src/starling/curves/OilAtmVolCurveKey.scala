package starling.curves

import starling.marketdata.{OilVolSurfaceDataKey, OilVolSurfaceData}
import starling.daterange._
import starling.maths.SplineInterpolator
import starling.quantity.{UOM, Percentage}
import starling.market.{Market, FuturesMarket, CommodityMarket}
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.quantity.Quantity
import starling.quantity.UOM

case class OilAtmVolCurveKey(market : CommodityMarket)
  extends NonHistoricalCurveKey[OilVolSurfaceData] with VolCurveKey {
  def marketDataKey = OilVolSurfaceDataKey(market.marketDataMarket)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: OilVolSurfaceData) =
    new OilAtmVol(marketDayAndTime, market, marketData)

  def volMarket = market
  override def higherUnderlying = market.commodity.name
}

case class OilAtmVolAtomicDatumKey(
  market : CommodityMarket,
  observationOrExerciseDay : Option[DateRange],
  period : DateRange,
  override val ignoreShiftsIfPermitted : Boolean = false
)
  extends AtomicDatumKey(
    OilAtmVolCurveKey(market),
    period
  ) with VolKey with EnvironmentDifferentiable{
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv(this)
  }
  def nullValue = Percentage(0.10)

  def periodKey = Some(period)

  def calc_dP(env : Environment) = new Quantity(0.50, UOM.PERCENT)
  def shiftedEnvs(env : Environment, dV_asQty : Quantity) : (Environment, Environment) = {
    val dV = dV_asQty.toPercentage
    val upEnv = env.shiftVol(market, observationOrExerciseDay, period, dV)
    val downEnv = env.shiftVol(market, observationOrExerciseDay, period, -dV)
    (downEnv, upEnv)
  }
  override def clearProperties : AtomicDatumKey = copy(ignoreShiftsIfPermitted = false)

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) : EnvironmentDifferentiable = {
    period match {
      case day : Day => {
        OilAtmVolAtomicDatumKey(
          market,
          observationOrExerciseDay,
          EnvironmentDifferentiable.containingDifferentiablePeriod(market.businessCalendar, marketDay, tenor, day),
          ignoreShiftsIfPermitted
        )
      }
      case _ => this
    }
  }
}

case class OilAtmVol(marketDayAndTime : DayAndTime, market : CommodityMarket, data : OilVolSurfaceData) extends CurveObject{

  def apply(point: AnyRef) = market.tenor match {
    case Month => {
      if (! data.periods.contains(point))
        throw new MissingMarketDataException("No oil atm for " + market + " - " + point + " in " + data)
      else
        data.atmVols(data.periods.indexOf(point))
    }
    case Day => {
      if (data.isDaily) {
        val times = data.periods.map(_.asInstanceOf[Day].endOfDay.timeSince(marketDayAndTime))
        val t = point.asInstanceOf[Day].endOfDay.timeSince(marketDayAndTime)
        Percentage(SplineInterpolator.interpolate(times, data.atmVols.map(_.decimalValue), t))
      } else {
        val day = point.asInstanceOf[Day]
        val index = data.periods.indexWhere {
          case m: Month => m.contains(day)
        }
        if(index == -1) throw new MissingMarketDataException("No oil atm for " + market + " - " + point + " in " + data)
        data.atmVols(index)
      }
    }
  }

  type CurveValuesType = Percentage
}

case class OilVolSkewCurveKey(market : CommodityMarket) extends NonHistoricalCurveKey[OilVolSurfaceData]{
  def marketDataKey = OilVolSurfaceDataKey(market.marketDataMarket)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: OilVolSurfaceData) = new OilVolSkew(marketDayAndTime, market, marketData)

  def underlying = market.name
}

case class OilVolSkewAtomicDatumKey(market : CommodityMarket, period : DateRange)
  extends AtomicDatumKey(new OilVolSkewCurveKey(market), period) {
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    originalAtomicEnv(this)
  }

  def nullValue = Map(0.5 -> Percentage(0))
}

case class OilVolSkew(marketDayAndTime : DayAndTime, market : CommodityMarket, data : OilVolSurfaceData) extends CurveObject{
  // Replace Percentages with Doubles as an optimization
  val skewsAsDoubles : Array[Array[Double]] = data.skews.map{
    arr => arr.map(_.decimalValue)
  }
  def skewMap(skews : Array[Double]) : DoubleMatrix2D = {
    val m = new DenseDoubleMatrix2D(skews.size, 2)
    m.viewColumn(0).assign(data.skewDeltas)
    m.viewColumn(1).assign(skews)
    m
  }
  def apply(point: AnyRef) = (point, market.tenor) match {
    case (m : Month, Month) => {
      if (data.months.contains(m))
        skewMap(skewsAsDoubles.map(_(data.months.indexOf(m))))
      else
        throw new MissingMarketDataException("No oil vol skew for " + market + " - " + m)
    }
    case (d: Day, Day) => {
      if (data.isDaily) {
        val times = data.days.map(_.endOfDay.timeSince(marketDayAndTime))
        val t = d.endOfDay.timeSince(marketDayAndTime)
        val skews = skewsAsDoubles.map(
          SplineInterpolator.interpolate(times, _, t)
        )
        skewMap(skews)
      } else {
        val index = data.periods.indexWhere {
          case m: Month => m.contains(d)
        }
        if (index == -1) throw new MissingMarketDataException("No oil vol skew for " + market + " - " + d)
        skewMap(skewsAsDoubles.map(_(index)))
      }

    }
  }

  type CurveValuesType = DoubleMatrix2D
}

