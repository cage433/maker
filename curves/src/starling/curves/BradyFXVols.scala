package starling.curves

import starling.quantity.Percentage
import starling.daterange.{Day, DayAndTime}
import starling.maths.SplineInterpolator
import starling.marketdata.{BradyFXVolSurfaceDataKey, BradyFXVolSurfaceData}
import starling.market.FXMarket
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.quantity.Quantity

case class BradyFXVolSmileKey(market : FXMarket)
  extends NonHistoricalCurveKey[BradyFXVolSurfaceData] with VolCurveKey {
  def marketDataKey = BradyFXVolSurfaceDataKey(market)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: BradyFXVolSurfaceData) =
    new BradyFXVolSmile(marketDayAndTime, market, marketData)

  def volMarket = market
}

case class BradyFXVolSmileAtomicDatumKey(market : FXMarket, day : Day)
  extends AtomicDatumKey(BradyFXVolSmileKey(market), day) with VolKey {
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = originalAtomicEnv(this)
  def nullValue = Map(0.5 -> Percentage(0))

  def underlying = market.name

  def volMarket = market

  def calc_dP(env : Environment) = new Quantity(0.0050)
  def shiftedEnvs(env : Environment, dV : Quantity) : (Environment, Environment) = {
    throw new Exception("Shifts not supported for brady fx vols")
  }
}

class BradyFXVolSmile(val marketDayAndTime : DayAndTime, market : FXMarket, data : BradyFXVolSurfaceData) extends CurveObject{
  def apply(point: AnyRef) : DoubleMatrix2D = point match {
    case d: Day => {
      val times = data.forwardDays.map(_.endOfDay.timeSince(marketDayAndTime))
      val t = d.endOfDay.timeSince(marketDayAndTime)
      def interpVolByTime(vols : Array[Percentage]) = SplineInterpolator.interpolate(times, vols, t).decimalValue
      val vols = data.vols.map(interpVolByTime(_))
      val m = new DenseDoubleMatrix2D(vols.size, 2)
      m.viewColumn(0).assign(data.deltas)
      m.viewColumn(1).assign(vols)
      m
    }
  }

  type CurveValuesType = DoubleMatrix2D
}
