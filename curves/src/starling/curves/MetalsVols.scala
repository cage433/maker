package starling.curves

import starling.quantity.Percentage
import starling.maths.SplineInterpolator
import starling.daterange.{DateRange, Month, Day, DayAndTime}
import starling.marketdata.{BradyMetalVolsDataKey, BradyMetalVolsData}
import starling.market.CommodityMarket
import starling.maths.interpolation.PolynomialInterpolator
import cern.colt.matrix.impl.DenseDoubleMatrix1D
import starling.utils.Log
import starling.quantity.Quantity
import starling.quantity.UOM
import starling.daterange.TenorType
import starling.daterange.Week

case class BradyMetalVolCurveKey(market : CommodityMarket)
  extends NonHistoricalCurveKey[BradyMetalVolsData] with VolCurveKey {
  def marketDataKey = BradyMetalVolsDataKey(market)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: BradyMetalVolsData) =
    new MetalsVols(marketDayAndTime, market, marketData)

  def volMarket = market
}

case class BradyMetalVolAtomicDatumKey(market : CommodityMarket, period : DateRange)
  extends AtomicDatumKey(BradyMetalVolCurveKey(market), period) with VolKey with EnvironmentDifferentiable{
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = originalAtomicEnv(this)
  def nullValue = Percentage(0.10)

  def volMarket = market

  def periodKey = Some(period)

  def underlying = market.name

  def calc_dP(env : Environment) = new Quantity(0.0050)
  def shiftedEnvs(env : Environment, dV_asQty : Quantity) : (Environment, Environment) = {
    val dV = Percentage(dV_asQty.checkedValue(UOM.SCALAR))
    val upEnv = env.shiftVol(market, None, period, dV)
    val downEnv = env.shiftVol(market, None, period, -dV)
    (downEnv, upEnv)
  }
  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = {
    period match {
      case day : Day => {
        BradyMetalVolAtomicDatumKey(
          market,
          EnvironmentDifferentiable.containingDifferentiablePeriod(market.businessCalendar, marketDay, tenor, day)
        )
      }
      case _ => this
    }
  }
}

class MetalsVols(val marketDayAndTime : DayAndTime, market : CommodityMarket, data : BradyMetalVolsData)
  extends CurveObject
{

  def apply(point: AnyRef) = (point, market.tenor) match {
    case (d : Day, Day) => {
      val t = d.endOfDay.timeSince(marketDayAndTime)
      val days = data.days.toArray
      val times = days.map(_.endOfDay.timeSince(marketDayAndTime))
      try {
        SplineInterpolator.interpolate(times, days.map(d=>Percentage.average(data.volsForPeriod(d).valuesIterator.toList)), t)
      } catch {
        case e:AssertionError => throw new MissingMarketDataException(e + " on " + market + " for " + data, e)
      }
    }
    case (m : Month, Month) => {
      import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
      val vols = data.volsForPeriod(m)
      val months = data.months.toArray
      val times = months.map(mm => 1.* (mm - m))
      try {
        val x = new DenseDoubleMatrix1D(times)
        val y = new DenseDoubleMatrix1D(months.map(m => Percentage.average(data.volsForPeriod(m).valuesIterator.toList).decimalValue))
        // TODO [27 May 2010] find how trinity inpterpolates. linear interpolation gets closest to what trinity value has
        val v = new PolynomialInterpolator(PolynomialInterpolator.LINEAR).interpolate(x, y, 0.0)
        Percentage(v)
      } catch {
        case e:AssertionError => throw new MissingMarketDataException(e + " on " + market + " for " + data, e)
      }
    }
  }

  type CurveValuesType = Percentage
}
