package starling.daterange

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class ObservationPointTests extends WordSpec with ShouldMatchers {
  import ObservationTimeOfDay._

  val observationDay = Day.today

  "can replace time of day" in {
    ObservationPoint(observationDay, Default).copyTime(Some(SHFEClose)) should be === ObservationPoint(observationDay, SHFEClose)
  }
}