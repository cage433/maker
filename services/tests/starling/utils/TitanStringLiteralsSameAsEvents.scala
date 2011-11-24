package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.trafigura.shared.events.Event._
import starling.titan.TitanStringLiterals._


class TitanStringLiteralsSameAsEvents extends WordSpec with ShouldMatchers {
  "overlapping 4fields have same values" in {
    starlingMarketDataSnapshotIDSubject      should be === StarlingMarketDataSnapshotIDSubject
    starlingNewValuationServiceStatusPayload should be === StarlingNewValuationServiceStatusPayload
    starlingObservationDay                   should be === StarlingObservationDay
    starlingReferenceRateSource              should be === StarlingReferenceRateSource
    starlingSnapshotIdPayload                should be === StarlingSnapshotIdPayload
    starlingSource                           should be === StarlingSource
    starlingSpotFXCurrency                   should be === StarlingSpotFXCurrency
    starlingValuationServiceSubject          should be === StarlingValuationServiceSubject
  }
}