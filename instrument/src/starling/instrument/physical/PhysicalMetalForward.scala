package starling.instrument.physical

import starling.curves.Environment
import starling.titan.valuation.QuotaValuation
import starling.utils.Log


case class PhysicalMetalForward(
  titanTradeID : String,
  quotas : List[PhysicalMetalQuota]) {

 /**
   * value trade quotas with their associated assignment values and adjusted quota portion values
   */
  def costsAndIncomeQuotaValueBreakdown(env: Environment) : Either[String, List[QuotaValuation]] = {

    try {
      Right(quotas.map {
        quota =>
          quota.value(env)
      })
    }
    catch {
      case ex => {
        Log.warn("Error valuing trade " + titanTradeID + ", message was " + ex.getMessage + "\n" + ex.getStackTrace)
        Left("Error valuing trade " + titanTradeID + ", message was " + ex.getMessage + "\n" + ex.getStackTrace)
      }
    }
  }
  def quotaIds = quotas.map(_.quotaID)
}
