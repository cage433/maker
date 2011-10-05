package starling.curves

import starling.marketdata.{SpotFXDataKey, SpotFXData}
import starling.quantity.{UOM, Quantity}
import starling.quantity.UOM.USD
import starling.daterange.{Day, DayAndTime}
import java.lang.String
import collection.immutable.Set
import starling.daterange.TenorType

case class USDFXRate(
  marketDayAndTime : DayAndTime,
  fxRate : Quantity
)
  extends CurveObject
{
  type CurveValuesType = Quantity
  private val numeratorCCY = fxRate.numeratorUOM
  private val denominatorCCY = fxRate.denominatorUOM
  require (numeratorCCY == USD && denominatorCCY != USD && denominatorCCY != UOM.NULL, "Rates must have units USD / ccy not " + fxRate.uom)

  def apply(point : AnyRef) : Quantity = fxRate
}

case class USDFXRateCurveKey(ccy : UOM) extends NonHistoricalCurveKey[SpotFXData]{
  def marketDataKey = SpotFXDataKey(ccy)
  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: SpotFXData) = {
    val fxRate = {
      (marketData.rate.uom.numeratorUOM, marketData.rate.uom.denominatorUOM) match {
        case (UOM.USD, _) => marketData.rate
        case (_, UOM.USD) => marketData.rate.invert
      }
    }
    USDFXRate(marketDayAndTime, fxRate)
  }

  def underlying = ccy.toString
}

case class USDFXRateKey(ccy : UOM)
  extends AtomicDatumKey(USDFXRateCurveKey(ccy), None) with EnvironmentDifferentiable
{
  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = {
    val env = Environment(originalAtomicEnv)
    env.forwardFXRate(USD, ccy, forwardDayAndTime.day)
  }

  def nullValue = Quantity(1, USD / ccy)

  def periodKey = None

  def calc_dP(env : Environment) = Quantity(0.01, USD / ccy)

  def shiftedEnvs(env : Environment, dS : Quantity) : (Environment, Environment) = {
    val upEnv = env.shiftSpotFX(ccy, dS)
    val downEnv = env.shiftSpotFX(ccy, -dS)
    (downEnv, upEnv)
  }

  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = this
}



