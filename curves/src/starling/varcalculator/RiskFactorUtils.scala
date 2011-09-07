package starling.varcalculator

import starling.market._
import starling.quantity.UOM
import starling.daterange._
import starling.curves.Environment

object RiskFactorUtils {

 

  /**
   * gets the appropriate vol risk factor for the given price risk factor
   */
  def volRiskFactor(pricePeriod: DateRange, riskFactors: Set[RiskFactor]): VolatilityRiskFactor = {
    val vrfs = volRiskFactors(riskFactors)

    // we can only handle 2 vol risk factors at the moment. if that becomes a problem this method
    // should be easy enough to rewrite, it was just much easier to write this way when there are currently
    // only ever 2 vol risk factors.
    assert(vrfs.size <= 2, "Too many vol risk factors")

    vrfs match {
      case (v@VolatilityRiskFactor(_, `pricePeriod`)) :: Nil => {
        // vol for the same period
        v
      }
      case (v@VolatilityRiskFactor(_, `pricePeriod`)) :: other => {
        // vol for same period with multiple vols
        v
      }
      case other :: (v@VolatilityRiskFactor(_, `pricePeriod`)) :: Nil => {
        // vol for same period with multiple vols
        v
      }
      case (v@VolatilityRiskFactor(_, _)) :: Nil => {
        //options can expire before futures we can have, for example, an asian
        // option that is sensitive to prices in May and Jun but only sensitive to
        // vol for Jun (as May vol is no longer available).
        // if this is the only vol risk factor then it is the right one
        v
      }
      case _ => throw new Exception("Shouldn't be here")
    }
  }

  def priceRiskFactors(riskFactors: Set[RiskFactor]): List[ForwardPriceRiskFactor] = riskFactors.filter {
    case _: ForwardPriceRiskFactor => true
    case _ => false
  }.toList.asInstanceOf[List[ForwardPriceRiskFactor]]

  def volRiskFactors(riskFactors: Set[RiskFactor]): List[VolatilityRiskFactor] = riskFactors.filter {
    case _: VolatilityRiskFactor => true
    case _ => false
  }.toList.asInstanceOf[List[VolatilityRiskFactor]]

  def hasVolSensitivity(riskFactors: Set[RiskFactor]) = !volRiskFactors(riskFactors).isEmpty
}
