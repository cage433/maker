package starling.market

import rules.{Precision, PrecisionRules, MarketPrecisionFactory}
import starling.daterange.{HolidaysSpec, TestHolidays}

trait PrecisionRulesSpec extends HolidaysSpec {
  MarketPrecisionFactory.registerNewRulesImplForTesting(
    Some(new PrecisionRules {
      def rule(eaiQuoteID: Int) = {
        eaiQuoteID match {
          case 890 => {// ICE WTI
            Some(Precision(4, 2))
          }
          case _ => None
        }
      }
    })
  )
}