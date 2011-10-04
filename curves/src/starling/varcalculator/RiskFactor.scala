package starling.varcalculator

import starling.daterange._
import starling.quantity.Quantity



object ForwardRateRiskFactor {

  def adjustment(marketDay: Day, period: DateRange, day: Day, dR: Quantity): Double = {
    if (day <= period.firstDay)
      1.0
    else {
      val time = (period.lastDay min day) daysSinceInYears period.firstDay
      math.exp(-dR.value * time)
    }
  }

}




