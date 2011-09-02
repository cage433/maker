package starling.varcalculator


import java.util.Arrays
import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.utils.conversions.RichColtMatrices._
import starling.instrument.Instrument
import cern.jet.math.Functions
import starling.quantity.utils.Summable

case class VarScenarioValues(values : Vector, errors:Set[Throwable]) extends Summable[VarScenarioValues]{
  def this(n : Int, t:Throwable) = this(new DVector(n), Set(t))
  def this(n : Int) = this(new DVector(n), Set[Throwable]())
  def + (rhs : VarScenarioValues) = VarScenarioValues(
    values.copy.assign(rhs.values, Functions.plus),
    errors ++ rhs.errors)
  def * (x : Double) = VarScenarioValues(values.copy.assign(Functions.mult(x)), errors)
  lazy val sortedValues = new DVector(values.toArray().sortWith(_<_))
  val nScenarios = values.size

  /**
   * Returns the value at risk - note that a centile of 0.95 means the (0.95 * nScenarios)th WORST value
   */
  def VaR(centile : Double) : Double = {
    if (values.size == 0)
      0.0
    else {
      val index = (nScenarios * (1.0 - centile)).asInstanceOf[Int]
      sortedValues(index)
    }
  }

  /**
   *  returns the cumulative VaR for a given centile - this is the average loss conditional on our
   *  finding ourselves in a scenario in that centile
   */
  def CVaR(centile : Double) : Double = {
    if (nScenarios == 0)
      0.0
    else {
      val index = (nScenarios * (1.0 - centile)).asInstanceOf[Int]
      sortedValues.viewPart(0, index).zSum  / index
    }
  }

  /**
   * Almost meaningless, but can be useful for giving a handle on differences in
   * unit tests
   */
  def standardError : Double = values.standardError

  def standardError(centile : Double) : Double = {
    if (nScenarios < 100)
      return 0.0
    val nSplits: Int = 30
    val splitScenarioVars = (0 until nSplits).toArray.map{
      i : Int =>
        val len = nScenarios / nSplits
        val start = i * len
        VarScenarioValues(values.viewPart(start, len min (nScenarios - start)), Set()).VaR(centile)
    }
    new DVector(splitScenarioVars).standardDeviation / math.sqrt(nSplits)
  }
}

object VarScenarioValues {
  def sum(size:Int, scenarios:Iterable[VarScenarioValues]) = {
    (new VarScenarioValues(size) /: scenarios) { (a,b) => a + b }
  }
}

