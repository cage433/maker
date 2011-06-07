package starling.curves

import starling.daterange.Day
import starling.daterange.DateRange
import starling.quantity.Quantity
import starling.quantity.Percentage
import cern.colt.matrix.DoubleMatrix2D
import starling.models.BlackScholes
import starling.models.Call
import starling.daterange.Month
import starling.daterange.DayAndTime
import starling.maths.LeastSquaresFit
import starling.daterange.Spread
import starling.curves.models.SpreadVolatilitySkew
import starling.quantity.UOM
import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}
import java.lang.reflect.InvocationTargetException
import starling.market._

/**
 * This class lies between Environment and AtomicEnvironment. It exists in order to perform perturbations
 * at a higher level than AtomicEnvironment. For example, shifting interpolated vols, rather than the 
 * original market data, shifting an expected fixation without changing the underlying. The latter is 
 * necessary to be able to sensibly report daily positions on frontline swaps.
 */ 

trait InstrumentLevelEnvironment extends AtomicEnvironmentHelper {
  def atomicEnv() : AtomicEnvironment

  def apply(key : AtomicDatumKey) : Any 
  def marketDay() : DayAndTime 
  def discount(ccy : UOM, day : Day, ignoreShiftsIfPermitted : Boolean = false) : Double
  def fixing(index : SingleIndex, fixingDay : Day) : Quantity

  def indexForwardPrice(index : SingleIndex, observationDay : Day, ignoreShiftsIfPermitted : Boolean = false) : Quantity 
  def interpolatedVol(market : HasImpliedVol, period : DateRange, exerciseDay : Option[Day], strike : Option[Quantity], isIndexVol : Boolean, forwardPrice: Option[Quantity]) : Percentage
  def spreadStdDev(market: FuturesMarket, spread : Spread[Month], exerciseDay: Day, strike: Quantity) : Quantity
  def indexVol(index : SingleIndex, observationDay : Day, strike : Quantity, averagePrice : Quantity) : Percentage
  def spreadPrice(market : FuturesMarket, firstMonth : Month, secondMonth : Month) : Quantity

  def shiftAtomicEnv(fn : AtomicEnvironment => AtomicEnvironment) : InstrumentLevelEnvironment
  def copy(newAtomicEnv : AtomicEnvironment) : InstrumentLevelEnvironment
  def shiftsCanBeIgnored  : Boolean
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) : InstrumentLevelEnvironment
}

class DefaultInstrumentLevelEnvironment(underlyingAtomicEnv : AtomicEnvironment)  extends InstrumentLevelEnvironment {
  // Parameterless constructor necessary for the dynamic proxys
  def this() = this(null)
  def atomicEnv() = {
    underlyingAtomicEnv
  }
  def apply(key : AtomicDatumKey) : Any = atomicEnv.apply(key)
  def shiftsCanBeIgnored = atomicEnv.shiftsCanBeIgnored
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = copy(atomicEnv.setShiftsCanBeIgnored(canBeIgnored))
  def shiftAtomicEnv(fn : AtomicEnvironment => AtomicEnvironment) = copy(fn(atomicEnv))
  def marketDay() : DayAndTime = atomicEnv().marketDay
  def discount(ccy : UOM, day : Day, ignoreShiftsIfPermitted : Boolean = false) : Double = atomicEnv.double(DiscountRateKey(ccy, day, ignoreShiftsIfPermitted))
  def fixing(index : SingleIndex, fixingDay : Day) : Quantity = {
    index.fixing(this, fixingDay)
  }

  def indexForwardPrice(index : SingleIndex, observationDay : Day, ignoreShiftsIfPermitted : Boolean = false) : Quantity = {
    index.forwardPrice(this, observationDay, ignoreShiftsIfPermitted)
  }

  def spreadPrice(market : FuturesMarket, firstMonth : Month, secondMonth : Month) = quantity(ForwardPriceKey(market, firstMonth)) - quantity(ForwardPriceKey(market, secondMonth))
  /**
   * Having to pass in the boolean flag 'inIndexVol' is a bit of a hack. The reason it is used is so that a key recording environment can catch
   * whether the vol we are getting is then used to create a swap vol, or a simple futures/forward vol. This is needed for reporting
   */
  def interpolatedVol(market : HasImpliedVol, period : DateRange, exerciseDay : Option[Day], strike : Option[Quantity], isIndexVol : Boolean, forwardPrice: Option[Quantity] = None) : Percentage = {
    assert(exerciseDay.isDefined == strike.isDefined, "exerciseDay and strike are defined badly (ex,str): " + (exerciseDay, strike))
    exerciseDay match {
      case Some(d) => assert(d.endOfDay > marketDay, "exericseDay " + d + " should be after market day: " + marketDay)
      case _ =>
    }

    def impliedOilVol(mkt : CommodityMarket, period: DateRange): Percentage = {
      // we need to use an atm vol that hasn't been perturbed in order to work out the strike delta
      val atmVolNoShifts = atomicEnv.percentage(OilAtmVolAtomicDatumKey(mkt, if(isIndexVol) exerciseDay else None, period, ignoreShiftsIfPermitted = true))
      val atmVol = atomicEnv.percentage(OilAtmVolAtomicDatumKey(mkt, if (isIndexVol) exerciseDay else None, period))
      val skewVol = strike match {
        case None => Percentage(0.0)
        case Some(strike) => {
          val skew: DoubleMatrix2D = atomicEnv.volMap(OilVolSkewAtomicDatumKey(mkt, period))
          // Hack to make Jon's tests work. We should really be using 'exerciseDay' here; the difference is v small though
          val expiryDay = if (isIndexVol){
            mkt match {
              case ke : KnownExpiry => ke.optionExpiry(period)
              case _ => period.firstDay - 1
            }
          } else {
            exerciseDay.get
          }
          val time = expiryDay.endOfDay.timeSince(marketDay)
          
          val disc = discount(mkt.currency, expiryDay, ignoreShiftsIfPermitted = true)
          val atmDelta = new BlackScholes(100.0, 100.0, Call, time, atmVolNoShifts).analyticDelta * disc
          // For Asian options we actually pass in the average price as the forward price
          val F = forwardPrice match {
            case Some(fp) => fp
            case None => atomicEnv.quantity(ForwardPriceKey(mkt, period, ignoreShiftsIfPermitted = true))
          }
          val strikeDelta = new BlackScholes(100 * F.value / strike.value, 100, Call, time, atmVolNoShifts).analyticDelta * disc
          LeastSquaresFit.fitVolSkew(3, atmDelta, skew)(strikeDelta)
        }
      }
      atmVol + skewVol
    }

    def impliedMetalVol(market: CommodityMarket, period: DateRange): Percentage = {
      atomicEnv.percentage(BradyMetalVolAtomicDatumKey(market, period))
    }

    def impliedFXVol(mkt : FXMarket, day : Day) : Percentage = {
      // this is ugly. we want to try to find the fx vols as USD/EUR or EUR/USD
      val smile = try {
        atomicEnv.volMap(new BradyFXVolSmileAtomicDatumKey(mkt, day))
      } catch {
        case m: MissingMarketDataException => try {
          atomicEnv.volMap(new BradyFXVolSmileAtomicDatumKey(mkt.inverse, day))
        }
        catch {
          case _: MissingMarketDataException => throw m
        }
      }
      // HACK - returning the atm vol instead of interpolating. this
      // means that this class doesn't need to access the forward fx method
      // in Environment - I'd rather not move it here if possible.
      // Justified by the fact that the vols in trinity are stale, often meaningless
      // and fx options are rarely traded at Traf
      val smilePoly = LeastSquaresFit.fitVolSmile(3, smile)
      smilePoly(0.5)
    }

    (market, period) match {
      case (mkt : FuturesMarket, period : DateRange) => {
        mkt.commodity match {
          case _: OilCommodity | NatGas => {
            impliedOilVol(mkt, period)
          }
          case _ => impliedMetalVol(mkt, period)
        }
      }
      case (mkt @ ProxyForwardMarket(futuresMarket), period : Day) =>
        interpolatedVol(futuresMarket, mkt.underlying(period), exerciseDay, strike, isIndexVol = false)

      case (mkt : ForwardMarket, period : Day) => {
        mkt.commodity match {
          case c : OilCommodity => {
            impliedOilVol(mkt, period)
          }
          case _ => impliedMetalVol(mkt, period)
        }
      }

      case (mkt : FXMarket, forwardDay : Day) => {
        impliedFXVol(mkt, forwardDay)
      }

    }
  }

  def spreadStdDev(market: FuturesMarket, spread : Spread[Month], exerciseDay: Day, strike: Quantity) = {
    val frontMonth = spread.first
    val backMonth = spread.last

    val time = exerciseDay.endOfDay.timeSince(marketDay)

    // need unshifted std dev from the market to get the delta of the corresponding option
    val atmSDNoShifts = atomicEnv.quantity(SpreadAtmStdDevAtomicDatumKey(market, spread, ignoreShiftsIfPermitted = true))
    // at-the-money standard deviation is the "base", then the interpolated skews are added to that.
    val atmSD = atomicEnv.quantity(SpreadAtmStdDevAtomicDatumKey(market, spread))

    val disc = discount(market.currency, exerciseDay, ignoreShiftsIfPermitted = true)
    //val disc = atomicEnv.double(DiscountRateKey(market.currency, exerciseDay, ignoreShiftsIfPermitted = true))
    val zeroRateNoShifts = -scala.math.log(disc) / time
    val discountFactorNoShifts = scala.math.exp(-zeroRateNoShifts * time)

    val F1 = atomicEnv.quantity(ForwardPriceKey(market, frontMonth, ignoreShiftsIfPermitted = true))
    val F2 = atomicEnv.quantity(ForwardPriceKey(market, backMonth, ignoreShiftsIfPermitted = true))
    val F = F1 - F2

    val skew: DoubleMatrix2D = atomicEnv.volMap(SpreadSkewStdDevAtomicDatumKey(market, spread))
    val callSkew = skew.get(0, 0)
    val putSkew = skew.get(0, 1)
    val volSurface = new SpreadVolatilitySkew(atmSD.value, atmSDNoShifts.value, zeroRateNoShifts, time, putSkew, callSkew, 2)
    val stdDev = volSurface.volatilityByStrike(strike.value, F.value)
    Quantity(stdDev, market.priceUOM)
  }

  def indexVol(index : SingleIndex, observationDay : Day, strike : Quantity, averagePrice : Quantity) = {
    index.volatility(this, observationDay, strike, averagePrice)
  }

  def copy(newAtomicEnv : AtomicEnvironment) = new DefaultInstrumentLevelEnvironment(newAtomicEnv)
  override def toString = "InstrumentLevelEnvironment " + getClass + ", atomic " + atomicEnv.getClass
}


object ShiftInstrumentLevelVol{

  val overridenMethodNames = List("atomicEnv", "indexVol", "interpolatedVol", "spreadStdDev", "copy")
  val List(atomicEnv, indexVol, interpolatedVol, spreadStdDev, copy) = overridenMethodNames

  val classMethodNames = classOf[DefaultInstrumentLevelEnvironment].getMethods.map(_.getName)
  require(overridenMethodNames.forall(classMethodNames.contains), "Invalid method name")
  
  def apply(originalEnv : InstrumentLevelEnvironment, diff : EnvironmentDifferentiable  with VolKey, dV : Quantity) : InstrumentLevelEnvironment = {

    val e = new Enhancer()
    e.setSuperclass(classOf[DefaultInstrumentLevelEnvironment])
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        try {

          def resultOnSelf = enhancedMethod.invokeSuper(enhancedObject, args)

          (originalMethod.getName, diff) match {
            case (`atomicEnv`, _) => {
              originalEnv.atomicEnv
            }

            case (`indexVol`, SwapVol(indexToShift, periodToShift)) => {
              args match {
                case Array(`indexToShift`, period : DateRange, _, _) if periodToShift.contains(period) => resultOnSelf.asInstanceOf[Percentage] + Percentage(dV)
                case Array(_, _, _, _) => resultOnSelf
              }
            }
            case (`spreadStdDev`, SpreadAtmStdDevAtomicDatumKey(market, periodToShift, _)) =>{
              args match {
                case Array(`market`, `periodToShift`, _, _) => resultOnSelf.asInstanceOf[Quantity] + dV
                case Array(_, _, _, _) => resultOnSelf
              }
            }
            case (`interpolatedVol`, OilAtmVolAtomicDatumKey(market, Some(observationDay), periodToShift, _)) => {
              args match {
                case Array(`market`, period : DateRange, Some(`observationDay`), _, _, _) if periodToShift.contains(period) => resultOnSelf.asInstanceOf[Percentage] + Percentage(dV)
                case Array(_, _, _, _, _, _) => resultOnSelf
              }
            }
            case (`interpolatedVol`, OilAtmVolAtomicDatumKey(market, None, periodToShift, _)) => {
              args match {
                case Array(`market`, period : DateRange, _, _, _, _) if periodToShift.contains(period) => resultOnSelf.asInstanceOf[Percentage] + Percentage(dV)
                case Array(_, _, _, _, _, _) => resultOnSelf
              }
            }
            case (`copy`, _) => ShiftInstrumentLevelVol(originalEnv.copy(args(0).asInstanceOf[AtomicEnvironment]), diff, dV)
            case _ => resultOnSelf
          }
        } catch {
          case e : InvocationTargetException => throw e.getCause
        }
      }
    })
    e.create().asInstanceOf[InstrumentLevelEnvironment]
  }
}

/**
 * This shifts a swap price based on its observation days, NOT any underlying period. Yes the great
 * gods of arbitrage are looking down and weeping, however it's pretty much impossible to show positions
 * as traders like them without this kind of hack.
 */
object ShiftSwapPrice{

  val overridenMethodNames = List("atomicEnv", "indexForwardPrice", "copy")
  val List(atomicEnv, indexForwardPrice, copy) = overridenMethodNames

  val classMethodNames = classOf[DefaultInstrumentLevelEnvironment].getMethods.map(_.getName)
  require(overridenMethodNames.forall(classMethodNames.contains), "Invalid method name")
  
  def apply(originalEnv : InstrumentLevelEnvironment, indexToShift : SingleIndex, periodToShift : DateRange, dP : Quantity) : InstrumentLevelEnvironment = {

    val e = new Enhancer()
    e.setSuperclass(classOf[DefaultInstrumentLevelEnvironment])
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        try {

          def resultOnSelf = enhancedMethod.invokeSuper(enhancedObject, args)

          originalMethod.getName match {
            case `atomicEnv` => {
              originalEnv.atomicEnv
            }

            case `indexForwardPrice` => {
              val originalPrice = resultOnSelf.asInstanceOf[Quantity]
              args match {
                case Array(`indexToShift`, d : Day, _) if periodToShift.contains(d) => originalPrice + dP
                case Array(_ : SingleIndex, _ : Day, _) => originalPrice
              }
            }
            case `copy` => ShiftSwapPrice(originalEnv.copy(args(0).asInstanceOf[AtomicEnvironment]), indexToShift, periodToShift, dP)
            case _ => resultOnSelf
          }
        } catch {
          case e : InvocationTargetException => throw e.getCause
        }
      }
    })
    e.create().asInstanceOf[InstrumentLevelEnvironment]
  }
}

object ParallelShiftInstrumentLevelVol{

  val overridenMethodNames = List("atomicEnv", "interpolatedVol", "spreadStdDev", "copy")
  val List(atomicEnv, interpolatedVol, spreadStdDev, copy) = overridenMethodNames

  val classMethodNames = classOf[InstrumentLevelEnvironment].getMethods.map(_.getName)
  require(overridenMethodNames.forall(classMethodNames.contains), "Invalid method name")
  
  def apply(originalEnv : InstrumentLevelEnvironment, curveKey : CurveKey, dV : Quantity) : InstrumentLevelEnvironment = {

    val e = new Enhancer()
    e.setSuperclass(classOf[DefaultInstrumentLevelEnvironment])
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        try {

          def resultOnSelf = enhancedMethod.invokeSuper(enhancedObject, args)

          (originalMethod.getName, curveKey.asInstanceOf[AnyRef]) match {
            case (`atomicEnv`, _) => {
              originalEnv.atomicEnv
            }

            case (`spreadStdDev`, SpreadAtmStdDevCurveKey(market)) =>{
              args match {
                case Array(`market`, _, _, _) => resultOnSelf.asInstanceOf[Quantity] + dV
                case Array(_, _, _, _) => resultOnSelf
              }
            }
            case (`interpolatedVol`, OilAtmVolCurveKey(market)) => {
              args match {
                case Array(`market`, _, _, _, _, _) => resultOnSelf.asInstanceOf[Percentage] + Percentage(dV)
                case Array(_, _, _, _, _, _) => resultOnSelf
              }
            }
            case (`copy`, _) => ParallelShiftInstrumentLevelVol(originalEnv.copy(args(0).asInstanceOf[AtomicEnvironment]), curveKey, dV)
            case _ => resultOnSelf
          }
        } catch {
          case e : InvocationTargetException => throw e.getCause
        }
      }
    })
    e.create().asInstanceOf[InstrumentLevelEnvironment]
  }
}

object ShiftMarketDayAtInstrumentLevel{
  def apply(originalEnv : InstrumentLevelEnvironment, newMarketDay : DayAndTime) : InstrumentLevelEnvironment = {
    
    val overridenMethodNames = List("atomicEnv", "marketDay", "discount", "fixing", "copy")
    val List(atomicEnv, marketDay, discount, fixing, copy) = overridenMethodNames
    assert(newMarketDay >= originalEnv.marketDay, "Can't shift time back")
    val forwardAtomicEnv = ForwardStateEnvironment(originalEnv.atomicEnv, newMarketDay)

    val e = new Enhancer()
    e.setSuperclass(classOf[InstrumentLevelEnvironment])
    e.setCallback(new MethodInterceptor() {
      def intercept(enhancedObject: Object, originalMethod: Method,
                    args: Array[Object], enhancedMethod: MethodProxy): Object = {
        try {

          originalMethod.getName match {
            case `atomicEnv` => {
              originalEnv.atomicEnv
            }

            case `marketDay` => newMarketDay
            case `discount` =>{
              args match {
                case Array(ccy : UOM, day : Day, ignoreShiftsIfPermitted) => forwardAtomicEnv.double(DiscountRateKey(ccy, day, ignoreShiftsIfPermitted.asInstanceOf[Boolean])).asInstanceOf[AnyRef]
              }
            }
            case `fixing` => {
              args match {
                case Array(index : SingleIndex, fixingDay : Day) =>
                  forwardAtomicEnv.quantity(FixingKey(index, fixingDay))
              }
            }
            case `copy` => ShiftMarketDayAtInstrumentLevel(originalEnv.copy(args(0).asInstanceOf[AtomicEnvironment]), newMarketDay)

            case _ => {
              originalMethod.invoke(originalEnv, args : _*)
            }
          }
        } catch {
          case e : InvocationTargetException => throw e.getCause
        }
      }
    })
    e.create().asInstanceOf[InstrumentLevelEnvironment]
  }
}

class CacheEnvironmentDifferentiables(originalEnv : InstrumentLevelEnvironment)  extends InstrumentLevelEnvironment{
  val cache = scala.collection.mutable.Set[EnvironmentDifferentiable]()

  def interpolatedVol(market : HasImpliedVol, period : DateRange, exerciseDay : Option[Day], strike : Option[Quantity], isIndexVol : Boolean, forwardPrice: Option[Quantity]) = {
    market match {
      case cm : CommodityMarket => cm.commodity match {
        case _ : OilCommodity => cache.add(OilAtmVolAtomicDatumKey(cm, exerciseDay, period))
        case _ : MetalCommodity => cache.add(BradyMetalVolAtomicDatumKey(cm, period))
      }
      case _ =>
    }
    originalEnv.interpolatedVol(market, period, exerciseDay, strike, isIndexVol, forwardPrice)
  }
  def spreadStdDev(market: FuturesMarket, spread : Spread[Month], exerciseDay: Day, strike: Quantity) = {
    cache.add(SpreadAtmStdDevAtomicDatumKey(market, spread))
    originalEnv.spreadStdDev(market, spread, exerciseDay, strike)
  }
  def indexVol(index : SingleIndex, observationDay : Day, strike : Quantity, averagePrice : Quantity) = {
    cache.add(SwapVol(index, observationDay))
    originalEnv.indexVol(index, observationDay, strike, averagePrice)
  }
  def indexForwardPrice(index : SingleIndex, observationDay : Day, ignoreShiftsIfPermitted : Boolean = false) : Quantity = {
    cache.add(SwapPrice(index, observationDay))
    originalEnv.indexForwardPrice(index, observationDay, ignoreShiftsIfPermitted)
  }
  def spreadPrice(market : FuturesMarket, firstMonth : Month, secondMonth : Month) = {
    cache.add(FuturesSpreadPrice(market, firstMonth, secondMonth))
    originalEnv.spreadPrice(market, firstMonth, secondMonth)
  }
  def atomicEnv = originalEnv.atomicEnv
  def apply(key : AtomicDatumKey) : Any = {
    key match {
      case ForwardPriceKey(market, period, _) => {
        cache.add(PriceDifferentiable(market, period))
      }
      case _ =>
    }
    originalEnv.apply(key)
  }
  def shiftsCanBeIgnored = originalEnv.shiftsCanBeIgnored
  def setShiftsCanBeIgnored(canBeIgnored : Boolean) = new CacheEnvironmentDifferentiables(originalEnv.setShiftsCanBeIgnored(canBeIgnored))
  def shiftAtomicEnv(fn : AtomicEnvironment => AtomicEnvironment) = new CacheEnvironmentDifferentiables(originalEnv.shiftAtomicEnv(fn))
  def marketDay() : DayAndTime = originalEnv.marketDay
  
  def copy(newAtomicEnv : AtomicEnvironment) = new CacheEnvironmentDifferentiables(originalEnv.copy(newAtomicEnv))
  def discount(ccy : UOM, day : Day, ignoreShiftsIfPermitted : Boolean = false) = originalEnv.discount(ccy, day, ignoreShiftsIfPermitted)
  def fixing(index : SingleIndex, fixingDay : Day) = originalEnv.fixing(index, fixingDay)
}
