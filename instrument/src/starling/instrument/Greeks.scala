package starling.instrument

import starling.models.DefaultRiskParameters
import starling.quantity.UOM._
import starling.daterange.TimeOfDay._
import starling.quantity.{Percentage, UOM, Quantity}
import starling.curves._
import starling.utils.AtomicDatumKeyUtils._
import starling.daterange.DayAndTime

case class UnsupportedDerivativeKeyException(atomicDatumKey: AtomicDatumKey) extends Exception("Key " + atomicDatumKey + " is not supported for calculating derivative")

trait Greeks {
  self: Instrument =>

  import Greeks._

  def cachedMtm(env : Environment, ccy : UOM) = env.greeksCache.memoize((this, ccy, "MTM"), mtm(env, ccy))
  /**
   * first order derivative given the atomic datum key.
   *
   * Note: this is the pure derivative without any adjustments to make it fit with what we would expect. For example
   * the derivative with respect to volatility is not the same as calling `vega` below. `vega` divides the result by 100.0
   */
  def firstOrderDerivative(
    env: Environment, 
    atomicDatumKey: EnvironmentDifferentiable, 
    ccy: UOM, 
    shiftInterpolatedVols : Boolean = false, 
    multiple : Double = 1.0
  ): Quantity = {
    val (_, upEnv, downEnv, shiftSize: Quantity) = shift(env, atomicDatumKey, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
    val upMtm = cachedMtm(upEnv, ccy)
    val downMtm = cachedMtm(downEnv, ccy)
    (upMtm - downMtm) / (shiftSize * 2.0)
  }

  /**
   * first order derivative given the atomic datum key.
   *
   * Note: this is the pure derivative without any adjustments to make it fit with what we would expect. For example
   * the derivative with respect to volatility is not the same as calling `vega` below. `vega` divides the result by 100.0
   */
  def firstOrderParallelDerivative(env: Environment, curveKey: CurveKey, ccy: UOM, shiftInterpolatedVols : Boolean): Quantity = {
    val (_, upEnv, downEnv, shiftSize: Quantity) = parallelShift(env, curveKey, shiftInterpolatedVols = shiftInterpolatedVols)
    val upMtm = cachedMtm(upEnv, ccy)
    val downMtm = cachedMtm(downEnv, ccy)
    (upMtm - downMtm) / (shiftSize * 2.0)
  }
  /**
   * second order derivative given the atomic datum key.
   *
   * No cross terms, use one of the more specific methods, such as gamma or vomma to get them.
   */
  def secondOrderDerivative(
    env: Environment, 
    atomicDatumKey: EnvironmentDifferentiable, 
    ccy: UOM, 
    shiftInterpolatedVols : Boolean = false, 
    multiple : Double = 1.0
  ): Quantity = {
    val (sameEnv, upEnv, downEnv, shiftSize: Quantity) = shift(env, atomicDatumKey, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
    val same = cachedMtm(sameEnv, ccy)
    val mtmUp = cachedMtm(upEnv, ccy)
    val mtmDown = cachedMtm(downEnv, ccy)
    (mtmUp - same * 2.0 + mtmDown) / (shiftSize * shiftSize)
  }

  def secondOrderParallelDerivative(env: Environment, curveKey: CurveKey, ccy: UOM, shiftInterpolatedVols : Boolean, multiple : Double = 1.0): Quantity = {
    val (sameEnv, upEnv, downEnv, shiftSize: Quantity) = parallelShift(env, curveKey, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
    val same = cachedMtm(sameEnv, ccy)
    val mtmUp = cachedMtm(upEnv, ccy)
    val mtmDown = cachedMtm(downEnv, ccy)
    (mtmUp - same * 2.0 + mtmDown) / (shiftSize * shiftSize)
  }

  /**
   * An optimization for PnlExplanation
   */
   def firstAndSecondOrderDerivatives(env : Environment, atomicDatumKey : EnvironmentDifferentiable, ccy : UOM) : (Quantity, Quantity) = {
    val (sameEnv, upEnv, downEnv, shiftSize: Quantity) = shift(env, atomicDatumKey)
    val same = cachedMtm(sameEnv, ccy)
    val mtmUp = cachedMtm(upEnv, ccy)
    val mtmDown = cachedMtm(downEnv, ccy)
    (
      (mtmUp - mtmDown) / (shiftSize * 2.0),
      (mtmUp - same * 2.0 + mtmDown) / (shiftSize * shiftSize)
    )
   }
  /**
   * second order derivative given the atomic datum key.
   *
   * Any cross term is added to the returned value.
   */
  def secondOrderDerivativeWithCrossTerm(
    env: Environment,
    atomicDatumKey: EnvironmentDifferentiable,
    ccy: UOM,
    allRelatedKeys : List[EnvironmentDifferentiable],
    shiftInterpolatedVols : Boolean = false, 
    multiple : Double = 1.0
  ): Quantity = {
    secondOrderDerivative(env, atomicDatumKey, ccy, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple) + crossTerm(env, allRelatedKeys, ccy, shiftInterpolatedVols = shiftInterpolatedVols)
  }

  def crossTerm(env: Environment, atomicDatumKeys: List[EnvironmentDifferentiable], ccy: UOM, shiftInterpolatedVols : Boolean, multiple : Double = 1.0): Quantity = {
    if (atomicDatumKeys.size == 1){
      // No cross term unless there is more than one thing to differentiate
      Quantity.NULL
    } else {
      val pGamma = secondOrderParallelDerivative(env, atomicDatumKeys.head.curveKey, ccy, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
      val gammas = atomicDatumKeys.map(secondOrderDerivative(env, _, ccy, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple))
      val crossGamma = (pGamma - gammas.sum) / gammas.size
      crossGamma
    }
  }

  /**
   * Parallel delta
   */
  def delta(env: Environment, ccy: UOM): Quantity = {
    val fpks = priceKeys(this, env.marketDay, ccy).toList
    assert(fpks.groupBy(_.market).size == 1, "Can't get parallel delta when there are multiple underyling markets: " + fpks)
    firstOrderParallelDerivative(env, fpks.head.curveKey, ccy, shiftInterpolatedVols = false)
  }

  /**
   * Delta by period
   **/
  def delta(env: Environment, atomicDatumKey: EnvironmentDifferentiable with PriceKey, ccy: UOM): Quantity = {
    val delta = firstOrderDerivative(env, atomicDatumKey, ccy)
    delta
  }

  /**
   * Delta bleed by period
   * Also called 'charm'
   **/
  def deltaBleed(env: Environment, atomicDatumKey: EnvironmentDifferentiable with PriceKey, thetaDayAndTime : DayAndTime, ccy: UOM): Quantity = {
    val (envToday, envThetaDay) = envFor(env, thetaDayAndTime)
    delta(envThetaDay, atomicDatumKey, ccy) - delta(envToday, atomicDatumKey, ccy)
  }

  /**
   * This is just parallel gamma
   */
  def centreGamma(env: Environment, ccy: UOM): Quantity = {
    val fpks = priceKeys(this, env.marketDay, ccy).toList
    assert(fpks.groupBy(_.market).size == 1, "Can't get centreGamma when there are multiple underyling markets: " + fpks)
    secondOrderParallelDerivative(env, fpks.head.curveKey, ccy, shiftInterpolatedVols = false)
  }

  /**
   * Gamma for each risk factor.
   * The gamma calculated will include any cross terms.
   */
  def gamma(env: Environment, atomicDatumKey: EnvironmentDifferentiable with PriceKey, ccy: UOM, allRelatedKeys : List[EnvironmentDifferentiable with PriceKey], multiple : Double = 1.0): Quantity = {
    atomicDatumKey match {
      // Cross terms is curve key gamma - sum(gammas). this doesn't work for for spread prices - I think it's ok for swap prices, but in any case they
      // should be for a single month.
      case _ : FuturesSpreadPrice => secondOrderDerivative(env, atomicDatumKey, ccy, multiple = 2.0 * multiple)
      case _ => secondOrderDerivativeWithCrossTerm(env, atomicDatumKey, ccy, allRelatedKeys, multiple = 2.0 * multiple)
    }
  }

  /**
   * Gamma bleed by period
   * Also called 'colour'
   **/
  def gammaBleed(env: Environment, atomicDatumKey: EnvironmentDifferentiable with PriceKey, thetaDayAndTime : DayAndTime, ccy: UOM, allRelatedKeys : List[EnvironmentDifferentiable with PriceKey]): Quantity = {
    val (envToday, envThetaDay) = envFor(env, thetaDayAndTime)
    gamma(envThetaDay, atomicDatumKey, ccy, allRelatedKeys) - gamma(envToday, atomicDatumKey, ccy, allRelatedKeys)
  }

  /**
   * 1-day theta
   */
  def oneDayTheta(env: Environment, ccy : UOM, changeOnlyTimeAndDiscounts : Boolean = false): Quantity = {
    theta(env, env.marketDay.nextDay, ccy, changeOnlyTimeAndDiscounts = changeOnlyTimeAndDiscounts)
  }
  /**
   * N-day theta
   * changeOnlyTimeAndDiscounts is used to emulte JF's theta. In this case only the time parameter of his models
   * changes (plus discounting), other functions of time - e.g. interpolated vols - are left unchanged. A similar
   * thing is done for vega calculations. The vega thing is reasonable, however doing this for theta sucks
   * and should be removed once JF moves to Starling style greeks.
   */
  def theta(env: Environment, thetaDayAndTime: DayAndTime, ccy : UOM, changeOnlyTimeAndDiscounts : Boolean = false): Quantity = {
    val (envToday, envTomorrow) = envFor(env, thetaDayAndTime, changeOnlyTimeAndDiscounts = changeOnlyTimeAndDiscounts)

    // theta including any interest
    val mtm1 = cachedMtm(envToday, ccy) // value at d (eod)
    val mtm2 = cachedMtm(envTomorrow, ccy) // value at d + 1 (start of day)
    mtm2 - mtm1
  }

  /**
   * parallel vega.
   *
   * first order derivative of price with respect to vol, divided by 100.0 in order to give a value for a 1% move in volatility
   * 
   * Use with care. For example this implementation would not work for asian options, that's why it's
   * overridden in AverageOption.
   **/
  def parallelVega(env: Environment, shiftInterpolatedVols : Boolean): Quantity = {
    val vks = volKeys(this, env.marketDay, valuationCCY).toList
    assert(vks.groupBy(_.curveKey).size == 1, "Can't get parallel vega when there are multiple underyling curve keys: " + vks)
    firstOrderParallelDerivative(env, vks.head.curveKey, valuationCCY, shiftInterpolatedVols = shiftInterpolatedVols) / 100.0
  }

  /**
   * first order derivative of price with respect to vol, divided by 100.0 in order to give a value for a 1% move in volatility
   */
  def vega(env: Environment, volKey: VolKey with EnvironmentDifferentiable, shiftInterpolatedVols : Boolean = false): Quantity = {
    val derivative: Quantity = firstOrderDerivative(env, volKey, valuationCCY, shiftInterpolatedVols = shiftInterpolatedVols)
    volKey match {
      // JF would like CSO vega to be the change in value for a 0.1$/bbl shift in the std dev
      case s : SpreadAtmStdDevAtomicDatumKey => derivative * s.calc_dP(env)
      case _ => derivative / 100.0
    }

  }

  /**
   * Second derivative of price with respect to volatility
   * Value returned includes any cross terms.
   *
   * divided by 10000.0 in order to give a value for a 1% move in volatility
   */
  def vomma(env: Environment, volKey: VolKey with EnvironmentDifferentiable, allRelatedKeys : List[VolKey with EnvironmentDifferentiable], shiftInterpolatedVols : Boolean = false): Quantity = {
    val derivative: Quantity = secondOrderDerivativeWithCrossTerm(env, volKey, valuationCCY, allRelatedKeys, shiftInterpolatedVols = shiftInterpolatedVols)
    volKey match {
      case s : SpreadAtmStdDevAtomicDatumKey => {
        val dP: Quantity = s.calc_dP(env)
        derivative * dP * dP
      }
      case _ => derivative / 10000
    }
  }

  private def shift(env: Environment, atomicDatumKey: EnvironmentDifferentiable, shiftInterpolatedVols : Boolean = false, multiple : Double = 1.0) : (Environment, Environment, Environment, Quantity) = {
    env.greeksCache.memoize((atomicDatumKey, shiftInterpolatedVols, multiple, "Shift"),{
      Greeks.shift(env, atomicDatumKey, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
    })
  }

  private def parallelShift(env: Environment, curveKey : CurveKey, shiftInterpolatedVols : Boolean, multiple : Double = 1.0) : (Environment, Environment, Environment, Quantity)= {
    //TODO [03 Dec 2010] find a way of memoizing on the curveKey and not its name - memoize doesn't like the type curveKey[_]
    env.greeksCache.memoize((curveKey.typeName + curveKey.underlying, shiftInterpolatedVols, multiple, "Parallel Shift"), {
      Greeks.parallelShift(env, curveKey, shiftInterpolatedVols = shiftInterpolatedVols, multiple = multiple)
    })
  }
  def priceKeysForTime(marketDay : DayAndTime) : Set[EnvironmentDifferentiable with PriceKey] = priceKeys(this, marketDay, valuationCCY).map(_.asInstanceOf[EnvironmentDifferentiable with PriceKey])

  def volKeysForTime(marketDay : DayAndTime) : Set[EnvironmentDifferentiable with VolKey] = volKeys(this, marketDay, valuationCCY).map(_.asInstanceOf[EnvironmentDifferentiable with VolKey])
}
object Greeks {
  val dVol = Percentage(0.0050)

  def dSpotFX(ccy: UOM) = Quantity(0.01, USD / ccy)

  def shift(
    env: Environment, 
    atomicDatumKey: EnvironmentDifferentiable, 
    shiftInterpolatedVols : Boolean = false, 
    multiple : Double = 1.0
  ) = {
    val riskEnv = env.copy(environmentParameters = DefaultRiskParameters)
    val dP = atomicDatumKey.calc_dP(env) * multiple
    val (dnEnv, upEnv) = if (atomicDatumKey.isInstanceOf[VolKey] && shiftInterpolatedVols) {
      val volDiff = atomicDatumKey.asInstanceOf[EnvironmentDifferentiable with VolKey]
      (
        riskEnv.shiftInterpolatedVol(volDiff, dP * -1),
        riskEnv.shiftInterpolatedVol(volDiff, dP)
      )
    } else {
      atomicDatumKey.shiftedEnvs(riskEnv, dP)
    }
    val List(s, u, d) = List(riskEnv, upEnv, dnEnv)
    (s, u, d, dP)
  }

  def parallelShift(env: Environment, curveKey: CurveKey, shiftInterpolatedVols : Boolean, multiple: Double = 1.0) = {
    val riskEnv = env.copy(environmentParameters = DefaultRiskParameters)
    val (upEnv, dnEnv, dP) = curveKey match {
      case f: ForwardCurveKey => {
        val dP = f.market.standardShift * multiple
        val upEnv = riskEnv.parallelShiftPrices(f.market, dP)
        val downEnv = riskEnv.parallelShiftPrices(f.market, -dP)
        (upEnv, downEnv, dP)
      }
      case o: VolCurveKey => {
        val dV = dVol * multiple
        if (shiftInterpolatedVols){
          (riskEnv.parallelShiftInterpolatedVols(curveKey, dV),
          riskEnv.parallelShiftInterpolatedVols(curveKey, -dV),
          dV : Quantity
        )
        } else {
          (riskEnv.parallelShiftVols(o.volMarket, dV), riskEnv.parallelShiftVols(o.volMarket, -dV), dV : Quantity)
        }
      }
      case u: USDFXRateCurveKey => {
        val dS = dSpotFX(u.ccy) * multiple
        val upEnv = riskEnv.shiftSpotFX(u.ccy, dS)
        val downEnv = riskEnv.shiftSpotFX(u.ccy, -dS)
        (upEnv, downEnv, dS)
      }
      case s: SpreadAtmStdDevCurveKey => {
        val dP = Quantity(0.001, s.market.priceUOM) * multiple
        val upEnv = riskEnv.parallelShiftSpreadStdDevs(s.market, dP)
        val downEnv = riskEnv.parallelShiftSpreadStdDevs(s.market, -dP)
        (upEnv, downEnv, dP)
      }
      case u => throw new Exception("Curve key " + curveKey + " not supported for parallel shifts")
    }
    val List(s, u, d) = List(riskEnv, upEnv, dnEnv)
    (s, u, d, dP)
  }

  def envFor(env:Environment, dayAndTime: DayAndTime, changeOnlyTimeAndDiscounts : Boolean = false) : (Environment, Environment) =
    env.greeksCache.memoize((dayAndTime, changeOnlyTimeAndDiscounts, "Forward State"),{
      val envToday = env.marketDay.timeOfDay match {
        case StartOfDay => env.forwardState(env.marketDay.copyTimeOfDay(EndOfDay))
        case EndOfDay => env
      }
      if (changeOnlyTimeAndDiscounts)
        (envToday, env.shiftMarketDayAtInstrumentLevel(dayAndTime))
      else
        (envToday, env.forwardState(dayAndTime))
    })
}
