package starling.market

import starling.calendar.{HolidayTablesFactory, BusinessCalendars}
import starling.daterange.DateRange
import starling.utils.Log

object FuturesExpiryRuleFactory {
  private var expiryRulesImpl : Option[FuturesExpiryRules] = None

	def registerRulesImpl(rules : FuturesExpiryRules){
		expiryRulesImpl match {
			case None => expiryRulesImpl = Some(rules)
			case Some(_) => throw new Exception("Implementation already registered")
		}
	}

  /**
   * Same as registerRulesImpl but allows new rules to be registerd
   */
  def registerNewRulesImplForTesting(rules : Option[FuturesExpiryRules]){
    expiryRulesImpl = rules
  }

  def expiryRuleOpton = expiryRulesImpl

	def expiryRules = {
		expiryRulesImpl match {
      // If you hit this from a test try extending the test class from TestExpiryRules
			case None => throw new Exception("Implementation not yet registered")
			case Some(rules) => rules
		}
  }
}