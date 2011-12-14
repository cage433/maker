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

  val baseEnvironment = environmentFor(curveIdentifier.observationDayAndTime)

  val environment = {
    if (curveIdentifier.valuationDayAndTime < curveIdentifier.observationDayAndTime) {
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

  def environmentFor(observationDayAntTime:DayAndTime) = {
    var env = Environment(atomicEnvironment(observationDayAntTime))
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

  def atomicEnvironment(dayAndTime:DayAndTime):AtomicEnvironment
  def thetaDayAndTime = curveIdentifier.thetaDayAndTime
}