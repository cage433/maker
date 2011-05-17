package starling.marketdata

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test


class RollingAveragePriceValidatorTests extends TestNGSuite with ShouldMatchers {
  import RollingAveragePriceValidator._

  @Test def movingAverageOfEmptyListIsAnEmptyList {
    movingAverage(Nil, 1) should be === Nil
    movingAverage(Nil, 2) should be === Nil
    movingAverage(Nil, 3) should be === Nil
  }

  @Test def movingAverageOfValuesSmallerThanWindowIsOriginalValues {
    movingAverage(List(1.0), 2) should be === List(1.0)
    movingAverage(List(1.0, 2.0), 3) should be === List(1.0, 2.0)
  }

  @Test def shouldFillAveragesWithHeadAndLastValue {
    movingAverage(List(1.0, 2.0, 3.0), 2) should be === List(1.5, 1.5, 2.5)
  }
}