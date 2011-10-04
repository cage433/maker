package starling.market

import starling.calendar.{HolidayTablesFactory, BusinessCalendars}
import starling.daterange.DateRange
import starling.utils.Log

object FuturesExpiryRuleFactory {
  private var expiryRulesImpl : Option[FuturesExpiryRules] = None

  private val lock = new Object
	def registerRulesImpl(rules : FuturesExpiryRules){
	  lock.synchronized {
      expiryRulesImpl match {
        case None => expiryRulesImpl = Some(rules)
        case Some(currentRules) if rules == currentRules => 
        case _ => throw new Exception("Implementation already registered")
      }
    }
	}

  def expiryRuleOpton = expiryRulesImpl

	def expiryRules: FuturesExpiryRules = {
		expiryRulesImpl match {
      // If you hit this from a test try extending the test class from TestExpiryRules
			case None => throw new Exception("Implementation not yet registered")
			case Some(rules) => rules
		}
  }
}
