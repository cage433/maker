package starling.daterange

import starling.daterange.Day._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class DayTest extends WordSpec with ShouldMatchers  {

  "should be correct start of year" in {
    (1 Oct 2011 startOfFinancialYear) should be === (1 Oct 2011)
    (3 Oct 2011 startOfFinancialYear) should be === (1 Oct 2011)
    (1 Jan 2012 startOfFinancialYear) should be === (1 Oct 2011)

    (29 Sep 2011 startOfFinancialYear) should be === (1 Oct 2010)
    
    (1 Oct 2012 startOfFinancialYear) should be === (1 Oct 2012)
  }
}