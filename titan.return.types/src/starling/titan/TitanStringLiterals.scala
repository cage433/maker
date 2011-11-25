package starling.titan

object TitanStringLiterals {
  val isValuationSnapshot = "Is Valuation Snapshot"

  // Cannot depend on the jar containing com.trafigura.shared.events.Event without depending on most of Titan, so duplicate & test
  val starlingMarketDataSnapshotIDSubject      = "starling market data snapshot id"
  val starlingNewValuationServiceStatusPayload = "ValuationStatus"
  val starlingObservationDay                   = "StarlingObservationDay"
  val starlingReferenceRateSource              = "StarlingReferenceRateSource"
  val starlingSnapshotIdPayload                = "StarlingSnapshotIdPayload"
  val starlingSource                           = "starling"
  val starlingSpotFXCurrency                   = "StarlingSpotFXCurrency"
  val starlingValuationServiceSubject          = "valuation service"

  val starlingSpotFXDataServiceSubject            = "StarlingSpotFXDataServiceSubject"
  val starlingReferenceInterestRateServiceSubject = "StarlingReferenceInterestRateServiceSubject"
}