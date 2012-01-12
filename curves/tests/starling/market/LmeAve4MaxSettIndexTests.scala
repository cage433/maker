package starling.market

import org.scalatest.matchers.ShouldMatchers
import starling.utils.StarlingSpec
import starling.daterange.{TimeOfDay, DayAndTime, Day}, Day._
import starling.curves.{AtomicDatumKey, TestingAtomicEnvironment, Environment}
import starling.quantity.{NamedQuantity, Quantity}

import starling.utils.ImplicitConversions._
import scalaz.Scalaz._


class LmeAve4MaxSettIndexTests extends StarlingSpec with ShouldMatchers {
  "price explanation should include min of 4 indicies" in {
    LmeAve4MaxSettIndex(market).provideFixingOrForwardPrice(env, today).asInstanceOf[NamedQuantity].format(1) should be ===
      "min(Average(16Jan2012, 16Jan2012, 12Apr2012, 12Apr2012), 16Jan2012)"
  }

  private lazy val today = 12 Jan 2012
  private lazy val market = Market.LME_ALUMINIUM
  private lazy val env = Environment(atomicEnv)

  private lazy val atomicEnv = new TestingAtomicEnvironment {
    def marketDay = DayAndTime(today, TimeOfDay.StartOfDay)
    def applyOrMatchError(key: AtomicDatumKey) = (Quantity(1.0, market.priceUOM)).named(key.toString)
  }
}