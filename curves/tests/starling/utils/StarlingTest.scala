package starling.utils

import org.scalatest.testng.TestNGSuite
import org.scalatest.WordSpec
import starling.market.{MarketProvider, TestMarketLookup, ExpiryRulesSpec, TestMarketSpec}

trait StarlingTest extends TestMarketSpec with ExpiryRulesSpec with TestNGSuite

trait StarlingSpec extends ExpiryRulesSpec with WordSpec {
  MarketProvider.registerImpl(TestMarketLookup)
}
