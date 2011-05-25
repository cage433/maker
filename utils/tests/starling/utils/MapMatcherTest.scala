package starling.utils

import org.testng.annotations.Test
import org.scalatest.matchers.ShouldMatchers
import starling.db.MapMatcher
import MapMatcher._


class MapMatcherTest extends ShouldMatchers {
  private val map = Map(1 -> 'a', 2 -> 'b')

  @Test
  def shouldPassWhenKeyAndValueMatch {
    map should containEntries(1 -> 'a')
  }

  @Test
  def shouldFailWhenKeyDoesntMatch {
    evaluating {
      map should containEntries(3 -> 'a')
    } should produce [Exception]
  }

  @Test
  def shouldFailWhenValueDoesntMatch {
    evaluating {
      map should containEntries(1 -> 'b')
    } should produce [Exception]
  }
}

