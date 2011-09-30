package starling.instrument.physical

import starling.curves.Environment
import starling.titan.valuation.QuotaValuation
import starling.daterange.Day
import starling.instrument._
import starling.utils.Log

case class PhysicalMetalForward(
  tradeID : String,
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
        Log.warn("Error valuing trade " + tradeID + ", message was " + ex.getMessage)
        Left("Error valuing trade " + tradeID + ", message was " + ex.getMessage)
      }
    }
  }
}
