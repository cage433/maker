package starling.marketdata

import starling.pivot._
import scalaz.Scalaz._
import starling.quantity.{Quantity, UOM}
import starling.pivot.Row._
case class FreightParityData(parityRate: Double, comment: String) extends MarketData {
  def size = 1
  def parityQuantity = Quantity(parityRate, UOM.USD / UOM.MT)
}

object FreightParityData{
  val ZERO = FreightParityData(0, "No freight parity exists")
}

case class FreightParityDataKey(contractualIncoterm: IncotermCode, contractualLocation: ContractualLocationCode,
  destinationIncoterm: IncotermCode, destinationLocation: NeptuneCountryCode) extends MarketDataKey {

  type marketDataType = FreightParityData
  type marketDataDBType = FreightParityData
  def typeName = MarketDataTypeName("FreightParity")
  def subTypeKey = ""
  def fields = Set("Contractual Incoterm", "Contractual Location", "Destination Incoterm", "Destination Location").map(Field(_))
}

class FreightParityDataType(referenceData: ReferenceDataLookup = ReferenceDataLookup.Null) extends MarketDataType {
  type dataType = FreightParityData
  type keyType = FreightParityDataKey

  val contractualIncotermCodeField = FieldDetails.coded("Contractual Incoterm", referenceData.incoterms.values)
  val contractualLocationCodeField = FieldDetails.coded("Contractual Location", referenceData.contractLocations.values)
  val destinationIncotermCodeField = FieldDetails.coded("Destination Incoterm", referenceData.incoterms.values)
  val destinationLocationCodeField = FieldDetails.coded("Destination Location", referenceData.countries.values)
  val extendedKeys = List(contractualIncotermCodeField, contractualLocationCodeField, destinationIncotermCodeField, destinationLocationCodeField)

  val values@List(parityRateField, commentField) =
    List(FieldDetails.createMeasure("Parity Rate", parser0 = PivotQuantityPivotParser), FieldDetails.createMeasure("Comment"))

  def valueFieldDetails = values

  override val zeroFields = List(parityRateField.field)
  val initialPivotState = PivotFieldsState(
    dataFields = valueFields,
    rowFields = keyFields.toList,
    filters = List( Field("Observation Time") -> SomeSelection(Set("Default")))
  )

  def createKey(row: Row) = FreightParityDataKey(
    IncotermCode(row.string(contractualIncotermCodeField)), ContractualLocationCode(row.string(contractualLocationCodeField)),
    IncotermCode(row.string(destinationIncotermCodeField)), NeptuneCountryCode(row.string(destinationLocationCodeField)))

  def createValue(rows: List[Row]): dataType = Row.singleRow(rows, "freight parity rate") |> { row =>
    FreightParityData(row.quantity(parityRateField).value, row.string(commentField))
  }

  protected def fieldValuesFor(key: FreightParityDataKey) = Row(
    contractualIncotermCodeField.field → key.contractualIncoterm.code,
    contractualLocationCodeField.field → key.contractualLocation.code,
    destinationIncotermCodeField.field → key.destinationIncoterm.code,
    destinationLocationCodeField.field → key.destinationLocation.code
  )

  def rows(key: FreightParityDataKey, data: FreightParityData) = List(
    fieldValues(key) +
    (parityRateField.field → data.parityQuantity) +
    (commentField.field → data.comment)
  )

  override val defaultValue = Row(parityRateField.field → Quantity(0, UOM.USD / UOM.MT), commentField.field → "")
}

case class ContractualLocationCode(code: String) {
  override def toString = code
}
case class ContractualLocation(code: ContractualLocationCode, name: String) extends Tupleable {
  def tuple = (code.code, name)
  override def toString = name
}
