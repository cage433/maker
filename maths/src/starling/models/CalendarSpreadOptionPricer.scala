package starling.models

import cern.jet.random.Normal
import scala.math._
import cern.jet.random.engine.MersenneTwister

class CalendarSpreadOptionPricer(val callPut: CallOrPut, val S: Double, val K: Double, val r: Double,
                                 val vol: Double, val T: Double) {
  require(T >= 0, "Negative time not allowed")
  require(vol >= 0, "Negative vol not allowed")
  
  val DF = exp(-r * T)
  val scaledVol = vol * sqrt(T)
  val d1 = (S - K) / scaledVol
  lazy val n1 = standardNormal.pdf(d1)
  lazy val N1 = standardNormal.cdf(d1)

  lazy val undiscountedValue = callPut match {
    case Call => scaledVol * n1 + (S - K) * N1
    case Put => scaledVol * n1 + (S - K) * (N1 - 1.0)
  }

  lazy val value = DF * undiscountedValue

  def standardNormal: Normal = new Normal(0.0, 1.0, new MersenneTwister(12345))
}