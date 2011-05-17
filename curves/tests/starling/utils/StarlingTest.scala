package starling.utils

import org.scalatest.testng.TestNGSuite
import starling.market.{ExpiryRulesSpec, TestExpiryRules}
import org.scalatest.WordSpec

trait StarlingTest extends ExpiryRulesSpec with TestNGSuite
trait StarlingSpec extends ExpiryRulesSpec with WordSpec
