package starling.services

import starling.gui.api._
/**
 * Locally store curves uploaded from xlloop and hand them out as price data.
 */
object PricingGroupIDUtil {
  def pricingGroupIDFor(marketDataSelection: MarketDataSelection): Int = {
    pricingGroupIDFor(marketDataSelection.pricingGroup)
  }

  def pricingGroupIDFor(pricingGroup: Option[PricingGroup]): Int = {
    pricingGroup match {
      case None => 4 //in case Jon uses excel only data
      case Some(PricingGroup.LondonDerivativesOptions) => 4
      case _ => throw new Exception("Only LondonDerivativesOptions supports pricing groups at the moment")
    }
  }
}