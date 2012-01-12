package starling.curves

import starling.marketdata.MarketData
import cern.colt.matrix.impl.DenseDoubleMatrix2D
import starling.utils.conversions.RichColtMatrices._
import cern.colt.matrix.linalg.Algebra
import starling.daterange.{Day, Month, Period, DateRange}
import starling.daterange.TenorType
import starling.daterange.Week
import starling.daterange.DayAndTime
import starling.daterange.TimeOfDay._
import starling.calendar.BusinessCalendar
import starling.market.{Index, PublishedIndex, CommodityMarket}
import starling.quantity.{UOM, Quantity}

trait EnvironmentDifferentiable{
  def curveKey : CurveKey
  def calc_dP(env : Environment) : Quantity
  def shiftedEnvs(env : Environment, dP : Quantity) : (Environment, Environment)
  def periodKey: Option[Period]
  def quantityValue (env : Environment) : Quantity

  def derivative(env : Environment, other : EnvironmentDifferentiable) : Quantity = {
    val dP = other.calc_dP(env)
    val (dnEnv, upEnv) = other.shiftedEnvs(env, dP)
    (quantityValue(upEnv) - quantityValue(dnEnv)) / (dP * 2.0)
  }
  def riskType : String = curveKey.typeName.name
  def riskCommodity : String = curveKey.higherUnderlying
  def riskMarket : String = curveKey.underlying
  def periods : List[DateRange] = periodKey.toList.flatMap(_.toList)
  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) : EnvironmentDifferentiable
}

object EnvironmentDifferentiable{
  def transform(
    env : Environment, 
    positions : Map[EnvironmentDifferentiable, Quantity],
    newDiffs : List[EnvironmentDifferentiable]
  ) : Map[EnvironmentDifferentiable, Quantity] = {

    if (positions.isEmpty)                                                        
      return Map()

    val uom = positions.head._2.uom
    assert(positions.forall(_._2.uom == uom), "Uom mismatch in " + positions)
    val oldDiffs = positions.keySet.toList
    val J = new DenseDoubleMatrix2D(oldDiffs.size, newDiffs.size)
    val V = new DenseDoubleMatrix2D(oldDiffs.size, 1)
    for ((oldDiff, i) <- oldDiffs.zipWithIndex){
      V(i, 0) = positions(oldDiff).value
      for ((newDiff, j) <- newDiffs.zipWithIndex){
        J(i, j) = newDiff.derivative(env, oldDiff).value
      }
    }
    val solution = Algebra.DEFAULT.solve(J, V)
    newDiffs.zipWithIndex.map{
      case (newDiff, i) => (newDiff, Quantity(solution(i, 0), uom))
    }.toMap
  }
  def toEnvironmentDifferentiable(key : AtomicDatumKey) : Option[EnvironmentDifferentiable] = {
    key.clearProperties match {
      case ForwardPriceKey(market, period, _) => Some(PriceDifferentiable(market, period))
      case ed : EnvironmentDifferentiable => Some(ed)
      case _ => None
    }
  }
  def applyBucketing(diffs : Set[EnvironmentDifferentiable]) : Set[EnvironmentDifferentiable] = {
    diffs.map{_ match {
      case PriceDifferentiable(market, day : Day) => PriceDifferentiable(market, day.containingMonth)
      case other => other
    }}
  }
  
  def containingDifferentiablePeriod(bc : BusinessCalendar, marketDay : DayAndTime, tenor : TenorType, day : Day) = {
    val newPeriod = tenor match {
      case `Week` => day.week
      case `Month` => day.containingMonth
      case _ => day
    }
  
    val newNewPeriod = {
      val firstDayWithPrice = marketDay match {
        case DayAndTime(d, `StartOfDay`) => bc.thisOrNextBusinessDay(d) 
        case DayAndTime(d, `EndOfDay`) => bc.thisOrNextBusinessDay(d + 1)
      }
      newPeriod.remainder(firstDayWithPrice).get
    } 
    newNewPeriod
  }
}

case class PriceDifferentiable(market : CommodityMarket, period : DateRange) extends EnvironmentDifferentiable with PriceKey{

  def quantityValue(env: Environment) = calculate(env).asInstanceOf[Quantity]

  def calculate(env : Environment) = {
    Index.getPublishedIndexForMarket(market) match {
      case Some(index) => env.averagePrice(index, period)
      case None => {
        market.tenor match {
          case Day => ForwardPriceKey(market, period.lastDay).quantityValue(env)
          case _ => ForwardPriceKey(market, period).quantityValue(env)
        }
      }
    }
  }

  def periodKey = Some(period)

  def shiftedEnvs(env : Environment, dP: Quantity) = {
    val downEnv = env.shiftPrice(market, period, -dP)
    val upEnv = env.shiftPrice(market, period, dP)
    (downEnv, upEnv)
  }

  def calc_dP(env: Environment) = market.standardShift

  def curveKey = ForwardCurveKey(market)
  def containingDifferentiable(marketDay : DayAndTime, tenor : TenorType) = {
    period match {
      case d : Day => 
        PriceDifferentiable(
          market,
          EnvironmentDifferentiable.containingDifferentiablePeriod(market.businessCalendar, marketDay, tenor, d)
        )
      case _ => this
    }
  }
}

