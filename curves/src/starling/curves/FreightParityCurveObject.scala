package starling.curves

import starling.daterange.DayAndTime
import starling.marketdata._

import starling.utils.ImplicitConversions._
import starling.quantity.{Quantity, UOM}

case class FreightParityAtomicKey(contractualIncoterm: IncotermCode, contractualLocation: ContractualLocationCode,
  destinationIncoterm: IncotermCode, destinationLocation: NeptuneCountryCode,
  override val ignoreShiftsIfPermitted: Boolean = false) extends AtomicDatumKey(FreightParityCurveKey(
    contractualIncoterm, contractualLocation, destinationIncoterm, destinationLocation), None, ignoreShiftsIfPermitted) {

  def forwardStateValue(originalAtomicEnv: AtomicEnvironment, forwardDayAndTime: DayAndTime) = originalAtomicEnv(this)
  def nullValue = new Quantity(0, UOM.USD / UOM.MT)
}

case class FreightParityCurveKey(contractualIncoterm: IncotermCode, contractualLocation: ContractualLocationCode,
  destinationIncoterm: IncotermCode, destinationLocation: NeptuneCountryCode) extends NonHistoricalCurveKey[FreightParityData] {
  def marketDataKey = FreightParityDataKey(contractualIncoterm, contractualLocation, destinationIncoterm, destinationLocation)

  def underlying = "%s %s %s %s Freight Parity" %
    (contractualIncoterm.code, contractualLocation.code, destinationIncoterm.code, destinationLocation.code)

  def buildFromMarketData(marketDayAndTime: DayAndTime, marketData: FreightParityData, refData : ReferenceDataLookup): FreightParityCurveObject =
    FreightParityCurveObject(marketDayAndTime, marketData)

  override def buildFromMarketData(marketDayAndTime: DayAndTime, marketDataSlice: MarketDataSlice, refData : ReferenceDataLookup): CurveObject = {
    try {
      super.buildFromMarketData(marketDayAndTime, marketDataSlice, refData)
    } catch {
      case _ : MissingMarketDataException =>
        buildFromMarketData(marketDayAndTime, FreightParityData.ZERO, refData)
    }
  }
}

case class FreightParityCurveObject(marketDayAndTime: DayAndTime, marketData: FreightParityData) extends CurveObject {
  type CurveValuesType = Quantity

  def apply(point: AnyRef) = marketData.parityQuantity
}
