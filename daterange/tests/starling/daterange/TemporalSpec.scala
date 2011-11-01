package starling.daterange

import starling.calendar.Clock
import org.scalatest.{Suite, BeforeAndAfterEach}

trait TemporalSpec extends BeforeAndAfterEach { self: Suite =>
  protected val frozenDay: Option[Day] = None

  override protected def beforeEach() = Clock.freezeTo(frozenDay)
  override protected def afterEach() = Clock.thaw
}