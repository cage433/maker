package starling.scheduler

import org.scalatest.matchers.ShouldMatchers
import starling.utils.conversions.RichLocalTime
import org.joda.time.Period
import starling.market.FuturesExchangeFactory
import starling.utils.StarlingSpec


class ScheduledTimeTests extends StarlingSpec with ShouldMatchers with RichLocalTime {
  import FuturesExchangeFactory._

  "8 o'clock @ LME != 8 o'clock @ SHFE" in {
    ScheduledTime("lme task", 8 H 00, Period.days(1), LME.calendar).scheduledTime should not be ===(
      ScheduledTime("lme task", 8 H 00, Period.days(1), SHFE.calendar).scheduledTime)
  }
}