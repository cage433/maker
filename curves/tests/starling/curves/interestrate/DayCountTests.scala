package starling.curves.interestrate

import starling.utils.StarlingTest
import org.testng.annotations.Test
import starling.maths.RandomVariables
import starling.daterange.{SimpleDateRange, Day}
import org.testng.Assert.assertEquals

class DayCountTests extends StarlingTest {


  @Test
  def testActualActualMatchesSimpleAlgorithmForManyPeriods(){
    val periods  = {
      val edgeCases = List(
        (Day(2009, 12, 31), Day(2010, 1, 1)),
        (Day(2008, 2, 27), Day(2008, 3, 1))
      )

      val x = RandomVariables.uniformDiscrete(4000, 12345)
      val y = RandomVariables.uniformDiscrete(500, 54321)
      val randomPeriods =  for (
        i <- 0 to 100;
        d1 = Day(2009, 1, 1) + x.nextInt();
        d2 = d1 + y.nextInt() + i
      )
        yield((d1, d2))
      (edgeCases ++ randomPeriods)
    }

    def oneDayFactor(d : Day) = if (d.containingYear.isLeapYear) 1.0 / 366 else 1.0 / 365

    /**
     * Check that act/act factor matches the rule
     *
     * factor = days not in leap year   +     Days in leap year
     *          --------------------          ----------------
     *                  365                         366
     *
     * where the first day is included, the last day is excluded 
     */
    def simpleActActFactor(d1 : Day, d2 : Day) : Double = {
      (0.0 /: SimpleDateRange(d1, d2 - 1).days.map(oneDayFactor)) (_+_)
    }
    periods.foreach{
      case (d1, d2) =>

        val actualValue = DayCountActualActual.factor(d1, d2)
        assertEquals(actualValue, simpleActActFactor(d1, d2), 1e-8)
    }
  }


  @Test
  def testRatioBetweenAct360AndActActIsConstantInSameYear{
    val periods = List(
      (Day(2009, 4, 3), Day(2009, 8, 2)),
      (Day(2008, 4, 3), Day(2008, 8, 2))
    )
    periods.foreach{
      case (d1, d2) =>

        val daysInYear = if (d1.containingYear.isLeapYear) 366.0 else 365.0
        val actActValue = DayCountActualActual.factor(d1, d2)
        val act360Value = DayCountActual360.factor(d1, d2)
        assertEquals(act360Value, actActValue / 360.0 * daysInYear, 1e-8)
    }
  }

  @Test
  def test30_360BehavesCorrectlyOnTheThirtyFirst{
    import DayCount30_360.factor
    val start = Day(2009, 5, 31)
    val end = Day(2009, 12, 31)
    assertEquals(factor(start - 1, end), factor(start, end), 1e-9)
    assertEquals(factor(start, end - 1), factor(start, end), 1e-9)
    assertEquals(factor(start - 1, end - 1), factor(start, end), 1e-9)
  }

  @Test
  def test30_360MakesAllMonths30Days{
    import DayCount30_360.factor
    assertEquals(factor(Day(2009, 1, 10), Day(2009, 2, 10)), 1.0 / 12, 1e-9)    
    assertEquals(factor(Day(2009, 1, 10), Day(2009, 2, 15)), 1.0 / 12 + 5.0 / 360, 1e-9)    
    assertEquals(factor(Day(2009, 1, 10), Day(2010, 5, 15)), 1.0 + 4.0 / 12 + 5.0 / 360, 1e-9)    
  }

  @Test
  def testDayCountParser{
    assertEquals(DayCount.fromTrinityCode("30/360"), DayCount30_360)
    assertEquals(DayCount.fromTrinityCode("Act'l/360"), DayCountActual360)
    assertEquals(DayCount.fromTrinityCode("Act'l/ACT'l"), DayCountActualActual)
  }
}
