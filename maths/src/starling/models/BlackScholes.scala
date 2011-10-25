package starling.models

import starling.quantity.Quantity
import starling.maths.RandomVariables._
import starling.utils.ImplicitConversions._
import math._
import cern.jet.random.Normal
import org.apache.commons.math.distribution.NormalDistribution

/** Standard Black-Scholes algorithm. Note the lack of discounting; any model which uses this
 *  must of course take care of that.
 *
 */
class BlackScholes(F : Double, X : Double, callPut : CallOrPut, T : Double, vol : Double) {
  require(T >= 0, "Negative time not allowed")
  require(vol >= 0, "Negative vol not allowed")

  private val isWorthIntrinsic = T == 0.0 || vol == 0.0
  val standardNormalInstance : Normal = standardNormal(12345)
  val standardNormalInstance2: NormalDistribution = standardNormal2

  private var N1 = 0.0
  private var n1 = 0.0
  private var N2 = 0.0

  lazy val d1 = (log(F / X) + vol * vol * T / 2.0 ) / (vol * sqrt(T))
  lazy val d2 = d1 - vol * sqrt(T)

  if (isWorthIntrinsic){
    callPut match {
      case Call if Call.isInTheMoney(X, F) => {
        N1 = 1.0
        N2 = 1.0
      }
      case Call =>
      case Put if Put.isInTheMoney(X, F) =>
      case Put => {
        N1 = 1.0
        N2 = 1.0
      }
    }
  } else {
    N1 = standardNormalInstance.cdf(d1)
    n1 = standardNormalInstance.pdf(d1)
    N2 = standardNormalInstance.cdf(d2)
  }
  val analyticDelta = callPut match {
    case Call => N1
    case Put => N1 - 1.0
  }
  val analyticVega = F * n1 * sqrt(T)

  val analyticGamma = n1 / (F * vol * sqrt(T))

  def strikeFromDelta(delta: Double) = {
    val n = standardNormal2.inverseCumulativeProbability(delta)
    F * exp(-n * vol * sqrt(T) + (vol*vol/2) * T)
  }

  // vanna is off by exactly a factor of 10. very strange
//  val analyticVanna = -n1 * d2 / vol

//  // Use Array rather than tuples to avoid boxing
//  private lazy val Array(n1, n2) =
//    if (isWorthIntrinsic){
//      callPut match {
//        case Call if Call.isInTheMoney(X, F) => Array(1.0, 1.0)
//        case Call => Array(0.0, 0.0)
//        case Put if Put.isInTheMoney(X, F) => Array(0.0, 0.0)
//        case Put => Array(1.0, 1.0)
//      }
//    } else {
//      val d1 = (log(F / X) + vol * vol * T / 2.0 ) / (vol * sqrt(T))
//      val d2 = d1 - vol * sqrt(T)
//      Array(standardNormalInstance.cdf(d1), standardNormalInstance.cdf(d2))
//    }

  lazy val probabilityOfExercise = callPut match {
    case Call => N2
    case Put => 1- N2
  }
  lazy val expectedPriceFractionGivenExercise = callPut match {
    case Call => N1
    case Put => 1 - N1
  }

  lazy val undiscountedOptionPrice = {
    callPut match {
      case Call => F * expectedPriceFractionGivenExercise - X * probabilityOfExercise
      case Put => -F * expectedPriceFractionGivenExercise + X * probabilityOfExercise
    }
  }

}

object BlackScholes extends OptionValuation{
  def undiscountedOptionPrice(F : Quantity, X : Quantity, callPut : CallOrPut, T : Double, vol : Double) : Quantity = {
    val (baseF, baseX) = (F.inBaseUOM, X.inBaseUOM)
    assert(baseF.uom == baseX.uom, "Unit mismatch, F = " + baseF + ", X = " + baseX)
    Quantity(
      undiscountedOptionPrice(baseF.value, baseX.value, callPut, T, vol),
      baseF.uom
    )
  }
  def undiscountedOptionPrice(F : Double, X : Double, callPut : CallOrPut, T : Double, vol : Double) : Double = {
    val bl = new BlackScholes(F, X, callPut, T, vol)
    bl.undiscountedOptionPrice
  }
  def valueUndiscounted(callOrPut: CallOrPut, F: Double, K: Double, sigma: Double, T: Double) = {
    undiscountedOptionPrice(F, K, callOrPut, T, sigma)
  }

  def value(callOrPut: CallOrPut, F: Double, K: Double, sigma: Double, r: Double, T: Double) = {
    undiscountedOptionPrice(F, K, callOrPut, T, sigma) * exp(-r * T)
  }
  def moneyAndStuff(F : Double, X : Double, callPut : CallOrPut, T : Double, vol : Double) : (Double, Double) = {
    val standardNormalInstance : Normal = standardNormal(12345)
    
    if (T == 0.0 || vol == 0.0){
      return callPut match {
        case Call => if (F > X) (-X, F) else (0, 0)
        case Put => if (F < X) (X, -F) else (0, 0)
      }
    }
    assert(T > 0, "Negative time supplied to option valuation")
    val d1 = (log(F / X) + vol * vol * T / 2.0 ) / (vol * sqrt(T))
    val d2 = d1 - vol * sqrt(T)
    val n1 = standardNormalInstance.cdf(d1)
    val n2 = standardNormalInstance.cdf(d2)
    callPut match {
      case Call => (F * n1, - X * n2)
      case Put =>  (- F * (1.0 - n1), X * (1.0 - n2))
    }
  }
  def moneyAndStuff(F : Quantity, X : Quantity, callPut : CallOrPut, T : Double, vol : Double) : (Quantity, Quantity) = {
    assert(F.uom == X.uom, "Unit mismatch")
    val (money, stuff) = moneyAndStuff(F.value, X.value, callPut, T, vol)
    (Quantity(money, F.uom), Quantity(stuff, F.uom))
  }

  override def toString = "Black Scholes"
}
