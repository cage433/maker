package starling.reports.impl

import pivot.CurveIdentifier
import starling.db._
import starling.curves._
import starling.marketdata._
import starling.daterange._
import starling.utils.ImplicitConversions._


abstract class AbstractReportContext(
        curveIdentifier:CurveIdentifier,
        environmentSliders:List[EnvironmentSlider]) extends ReportContext {

  def recorded:Set[(ObservationPoint, MarketDataKey, MarketData)]

  val baseEnvironment = environmentFor(ObservationDay(curveIdentifier.observationDay))

  val environment = {
    if (curveIdentifier.valuationDayAndTime.day < curveIdentifier.observationDay) {
      throw new Exception("The valuation day can not be moved backwards")
    }
    if (curveIdentifier.valuationDayAndTime == baseEnvironment.marketDay) {
      baseEnvironment
    } else if (curveIdentifier.valuationDayAndTime > baseEnvironment.marketDay) {
      baseEnvironment.forwardState(curveIdentifier.valuationDayAndTime)
    } else {
      throw new IllegalStateException("Can't move the valuation day back: " + (curveIdentifier.valuationDayAndTime, baseEnvironment.marketDay))
    }
  }

  val marketDayAndTime = environment.marketDay

  def environmentFor(observationDay:ObservationDay) = {
    var env = Environment(atomicEnvironment(observationDay.day))
    for (environmentSlider <- environmentSliders) {
      env = environmentSlider.slide(env)
    }
    modifyEnv(env)
  }
  private def modifyEnv(env:Environment) = {
    var myEnv = env
    if (curveIdentifier.zeroInterestRates) {
      myEnv = env.undiscounted
    }
    if (curveIdentifier.zeroVols) {
      myEnv = myEnv.zeroVols
    }
    myEnv
  }

  def atomicEnvironment(day:Day):AtomicEnvironment
  def thetaDayAndTime = curveIdentifier.thetaDayAndTime
}