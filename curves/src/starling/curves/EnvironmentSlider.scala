package starling.curves

import starling.daterange.DayAndTime
import starling.quantity.{Percentage, Quantity}
import starling.market.{Commodity, FuturesMarket, CommodityMarket}

object SlideType extends Enumeration {
  type SlideType = Value
  val Price = Value("Price (market)")
  val PriceCommodity = Value("Price (commodity)")
  val StressCommodity = Value("Stress (commodity)")
  val Vols = Value("Vols")
  val StdDev = Value("StdDev")
  val Time = Value("Time")
}

/**
 * Slides an environment and provides information about it.
 */
trait EnvironmentSlider {
  def slide(env:Environment):Environment
  def stepNumber:Int
}
case class PriceSlideParameters(market:CommodityMarket, dP:Quantity, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = env.parallelShiftPrices(market, dP)
}
case class PriceCommoditySlideParameters(commodity:Commodity, dP:Quantity, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = env.parallelShiftPrices(commodity, dP)
}
case class StressCommoditySlideParameters(commodity:Commodity, dP:Quantity, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = env.parallelStressShiftPrices(commodity, dP)
}
case class StdDevSlideParameters(market:FuturesMarket, dP:Quantity, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = env.parallelShiftSpreadStdDevs(market, dP)
}
case class VolsSlideParameters(market:CommodityMarket, dP:Percentage, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = env.parallelShiftVols(market, dP)
}
case class TimeSlideParameters(daysToSlide:Int, stepNumber:Int) extends EnvironmentSlider {
  def slide(env:Environment) = {
    val marketDay = env.marketDay
    val newDayAndTime = DayAndTime(marketDay.day + daysToSlide, marketDay.timeOfDay)
    env.forwardState(newDayAndTime)
  }
}
case class SpotFXSlideParameters(dP:Quantity, stepNumber:Int) extends EnvironmentSlider {
  // TODO [24 May 2010]
  def slide(env:Environment) = env
}
case object DoNotSlideSlideParameters extends EnvironmentSlider {
  def slide(env:Environment) = env
  def stepNumber = 0
}

