package starling.curves

import starling.utils.StarlingTest
import starling.quantity.UOM._
import starling.utils.QuantityTestUtils._
import org.testng.annotations.{DataProvider, Test}
import starling.daterange.{Year, Quarter, Month, DateRange}
import starling.maths.RandomVariables
import starling.quantity.{UOM, Quantity}

class FreightCurveTests extends StarlingTest {

  val dr = RandomVariables.uniformDiscrete(3, 12345)
  val y = RandomVariables.uniformDiscrete(10, 54321)
  val m = RandomVariables.uniformDiscrete(12, 541)
  val q = RandomVariables.uniformDiscrete(4, 6785)
  val p = RandomVariables.standardUniform(998877)

  def randomPeriodAndPrice() = {
    val period = dr.nextInt match {
      case 0 => Year(2000 + y.nextInt)
      case 1 => Month(2000 + y.nextInt, 1 + m.nextInt)
      case 2 => Quarter(2000 + y.nextInt, 1 + q.nextInt)
    }
    val price = p.nextDouble
    period → price
  }

  def randomRawPrices = {
    val n = m.nextInt * 2 + 1
    (0 until n).toList.map{i => randomPeriodAndPrice}.toMap
  }

  @DataProvider(name = "rawPrices")
  def rawPrices = {
    (Array(
      Map(Month(2009, 1) → 1.0),
      Map(
        Month(2009, 1) → 1.0,
        Quarter(2009, 1) → 2.0
      ),
      Map(
        Month(2009, 1) → 1.0,
        Quarter(2009, 1) → 2.0,
        Year(2009) → 3.0
      ))
      ++
      (0 until 10).toArray.map{i => randomRawPrices}

    ).map{ map => Array(map.asInstanceOf[Map[DateRange, Quantity]])}
  }

  @Test(dataProvider = "rawPrices")
  def testSimpleCurve(rawPrices: Map[DateRange, Double]) {
    import Quantity._

    val (monthPrices, ignoredPeriods) = FreightCurve.calcMonthlyPricesFromArbitraryPeriodsWithUnusedPeriodList(rawPrices)
    assert(ignoredPeriods.size * 5 <= monthPrices.size, "Ignored too much")
    def averagePrice(dr : DateRange) = {
      val months = dr.toListOfMonths

      months.map(monthPrices).sum / months.size
    }
    rawPrices.foreach { case (period, price) =>
      if (!ignoredPeriods.contains(period)) {
        assertQtyEquals(price, averagePrice(period), 1e-6)
      }
    }
  }

}
