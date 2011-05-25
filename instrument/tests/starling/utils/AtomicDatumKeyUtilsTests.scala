package starling.utils

import org.testng.annotations.Test
import starling.daterange.Day._
import starling.quantity.Quantity
import starling.quantity.UOM._
import org.testng.Assert._
import starling.curves._
import starling.models.{American, European, Call}
import starling.daterange.{Day, Month}
import starling.instrument.{SingleAsianOption, AsianOption, ForwardOption, FuturesOption}
import starling.instrument.UTP
import starling.market.{PublishedIndex, FuturesFrontPeriodIndex, TestExpiryRules, Market}

class AtomicDatumKeyUtilsTests extends TestExpiryRules {
  @Test
  def testBucketing {
    val index = PublishedIndex.PREM_UNL_EURO_BOB_OXY_NWE_BARGES
    val market = index.market

    val dec10 = Month(2010, 12)
    val jan11 = Month(2011, 1)
    val option = new SingleAsianOption(index, dec10, Quantity(100, USD / MT), Quantity(100, MT), Call)

    val (priceKeys, volKeys) = UTP.priceAndVolKeys(option, (1 Nov 2010).endOfDay, showEquivalentFutures = false, tenor = Day)
    val keys = priceKeys ++ volKeys

    val expectedKeys = Set() ++ index.observationDays(dec10).map(SwapPrice(index, _)) ++ index.observationDays(dec10).map(SwapVol(index, _))

    assertEquals(keys, expectedKeys)
  }

  @Test
  def testBucketingWhenNotNeeded {
    val market = Market.NYMEX_WTI
    val dec10 = Month(2010, 12)
    val exerciseDay = market.optionExpiry(dec10)
    val option = new FuturesOption(market, exerciseDay, dec10, Quantity(100, USD / BBL), Quantity(100, BBL), Call, European)

    val keys = AtomicDatumKeyUtils.atomicDatumKeys(option, (1 Nov 2010).endOfDay, USD)
    val bkeys = EnvironmentDifferentiable.applyBucketing(AtomicDatumKeyUtils.environmentDifferentiables(option, (1 Nov 2010).endOfDay, USD))
    assertEquals(bkeys,
      Set(OilAtmVolAtomicDatumKey(market, None, dec10, false), PriceDifferentiable(market, dec10)))
  }
}
