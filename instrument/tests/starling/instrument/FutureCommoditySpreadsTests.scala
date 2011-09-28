package starling.instrument


import starling.curves._
import starling.utils._
import starling.instrument.utils._
import starling.daterange.{Day, Month}
import starling.daterange.Day._
import starling.quantity.UOM._
import starling.quantity.RichQuantity._
import starling.quantity.utils.QuantityTestUtils._
import starling.quantity.{UOM, Quantity}
import starling.market._


class FutureCommoditySpreadsTests extends JonTestEnv {
  import org.testng.annotations._
  import org.testng.Assert._

  val mdt = (1 Jan 2010).endOfDay
  val env = makeEnv(mdt)
  
  val market1 = Market.NYMEX_GASOLINE
  val market2 = Market.NYMEX_WTI
  val crack = FuturesSpreadMarket.RB_CRACKS
  val period = Month(2011, 3)
  lazy val price1 = env.forwardPrice(market1, period)
  lazy val price2 = env.forwardPrice(market2, period)

  @Test
  def testFuturesMtm {
    val spread = Quantity(-1, USD/BBL)

    val f1 = Future(market1, period, spread.copy(value = 0.0), 1(BBL))
    val f2 = Future(market2, period, -spread, (-1)(BBL))
    val fs = new FuturesCommoditySpread(crack, period, spread, 1(BBL))

    val fmtm = f1.mtm(env) + f2.mtm(env)
    val mtm = fs.mtm(env)
    assertQtyEquals(mtm, fmtm, 1e-6)
  }
  

  @Test
  def testSensitivities {
    val spread = Quantity(-1, USD/BBL)

    val f1 = Future(market1, period, spread.copy(value = 0.0), 1(BBL))
    val f2 = Future(market2, period, -spread, (-1)(BBL))
    val fs = new FuturesCommoditySpread(crack, period, spread, 1(BBL))

    val f1ds = AtomicDatumKeyUtils.environmentDifferentiables(f1, mdt, USD)
    val f2ds = AtomicDatumKeyUtils.environmentDifferentiables(f2, mdt, USD)
    val fsds = AtomicDatumKeyUtils.environmentDifferentiables(fs, mdt, USD)

    assertEquals(fsds, f1ds ++ f2ds)

    fsds.map {
      s => {
        val differentiable = s.asInstanceOf[EnvironmentDifferentiable with PriceKey]
        val delta = fs.delta(env, differentiable, USD)
        assertTrue(delta != 0, "delta: " + delta)
        assertQtyEquals(delta, f1.delta(env, differentiable, USD) + f2.delta(env, differentiable, USD), 1e-9)
      }
    }
  }


}