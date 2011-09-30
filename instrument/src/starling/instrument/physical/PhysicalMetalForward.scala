package starling.instrument.physical

import starling.curves.Environment
import starling.titan.valuation.QuotaValuation
import starling.instrument.Trade


case class PhysicalMetalForward(
  tradeID : String,
  groupCompany : String,
  counterparty : String,
  quotas : List[PhysicalMetalQuota],
  isPurchase : Boolean
){
 /**
   * value trade quotas with their associated assignment values and adjusted quota portion values
   */
  def costsAndIncomeQuotaValueBreakdown(env: Environment, snapshotID: String) : List[QuotaValuation] = {
    quotas.map {
      quota =>
        quota.value(env)
    }
  }

  def asStarlingTrades : List[Trade] = {
    Nil
  }
}
