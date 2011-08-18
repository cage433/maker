package starling.utils

import org.scalatest.testng.TestNGSuite
import org.scalatest.WordSpec
import starling.market.{MarketProvider, TestMarketLookup, ExpiryRulesSpec, TestMarketTest}

trait StarlingTest extends TestMarketTest with ExpiryRulesSpec with TestNGSuite

trait StarlingSpec extends ExpiryRulesSpec with WordSpec {
  MarketProvider.registerImpl(TestMarketLookup)
}
