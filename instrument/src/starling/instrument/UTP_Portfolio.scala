package starling.instrument

import starling.varcalculator._
import starling.daterange.Day
import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector}
import starling.quantity.{Quantity, UOM}
import starling.curves.Environment

/** A UTP_Portfolio exists to describe a general portfolio of instruments in term of volumes of unique trade parameters.
 *  This exists as an optimization. For mtm/risk/VAR purposes we don't care about the individual trades in a particular
 * 	futures contract, just the net position. 
 */
case class UTP_Portfolio(portfolio : Map[UTP, Double]) extends Instrument {//} with Summable[Instrument, UTP_Portfolio]{
	def this() = this(Map.empty[UTP, Double])

  def pivotUTPType = throw new Exception("Unimplemented")


	/** Returns the valuation currency for the instruments in this portfolio. Fails
  * 	if the portfolio is empty or if there is a mixture of currencies.
  */
  def valuationCCY : UOM = {
		assert(!portfolio.isEmpty, "Empty portfolios have no currency")
		val ccy = portfolio.keySet.iterator.next.valuationCCY
		assert(portfolio.keySet.forall(_.valuationCCY == ccy), "Portfolio of mixed currency deals has no currency")
		ccy
  }

  def explanation(env: Environment) = throw new Exception("No explanation for UTP Portfolio")

  /** Values the portfolio - will fail if instruments have different currencies
   */
  override def mtm(env : Environment) : Quantity = {
			if (portfolio.isEmpty)
				Quantity.NULL
			else
				mtm(env, valuationCCY)
	}

  def assets(env : Environment) = {
    var a = Assets()
    portfolio.map {
      case (inst, volume) =>
        a = a ++ (inst.assets(env) * volume)
    }
    a
  }

  def instrumentType = UTP_Portfolio

  /** Values the portfolio using the given currency
   */
  override def mtm(env : Environment, ccy : UOM) : Quantity = {
			var mtm = Quantity.NULL
			portfolio.map{
				case (inst, volume) => 
				  mtm = mtm + inst.mtm(env, ccy) * volume
			}
			mtm
	}

  def ++(other:UTP_Portfolio) = {
    val allKeys = portfolio.keySet ++ other.portfolio.keySet
    UTP_Portfolio(Map() ++ allKeys.map{instrument=>instrument->(portfolio.getOrElse(instrument, 0.0) + other.portfolio.getOrElse(instrument, 0.0))})
  }
 
	/** Return a new UTP_Portfolio by adding a single instrument to this one
  */
	def add(instrument : Tradeable, tradeDay:Day) : UTP_Portfolio = {
    var newPortfolio = portfolio
    val instUtp = instrument.asUtpPortfolio(tradeDay)
    instUtp.portfolio.foreach{
      case (standardInstrument, volume) =>
        newPortfolio += (standardInstrument -> (portfolio.getOrElse(standardInstrument, 0.0) + volume))
    }
	  UTP_Portfolio(newPortfolio)
	}
	def size : Int = portfolio.size 

	def instruments : Set[UTP]= Set.empty ++ portfolio.keySet


  def describe{
	  val fmt = new java.text.DecimalFormat("0.00")
	  portfolio.keySet.toList.sortWith(_<_).foreach{
	    inst =>
	    	println("\t\t" + inst)
	    	println("\t\t\t" + fmt.format(portfolio(inst)))

	  }
	}
  def details = Map[String, Any]()


  def asUtpPortfolio = this

  def removeNetZeros = {
//    UTP_Portfolio(Map() ++ (for((k,v) <- portfolio if v.abs > MathUtil.EPSILON) yield {
//      (k -> v)
//    }))
    this
  }

  def isEmpty = portfolio.isEmpty

  /**
   * Overrides as a UTP portfolio doesn't have a valuation currency, which is a risk factor
   */
  override def riskFactors(env: Environment, ccy: UOM) = {
    (Set[RiskFactor]() /: instruments.map(_.riskFactors(env, ccy)))(_++_)
  }

  override def toString = portfolio.toList.sortWith(_._1 < _._1).map{
    case (utp, volume) => utp + " -> " + volume
  }.mkString("Portfolio\n\t", "\n\t", "")
}

object UTP_Portfolio {

 /** Builds a UTP_Portfolio from a collection of instruments
  */
	def build(insts : Seq[Tradeable], tradeDay:Day) : UTP_Portfolio = {
	  (new UTP_Portfolio /: insts) { (p,i) => p.add(i,tradeDay) }
	}

  val empty = UTP_Portfolio(Map())
}
