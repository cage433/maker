package starling.marketdata

import starling.pivot._
import scalaz.Scalaz._
import starling.quantity.{Quantity, UOM}
import starling.pivot.Row._

case class FreightParityData(parityRate: Double, comment: String) extends MarketData {
  def size = 1
  def parityQuantity = Quantity(parityRate, UOM.USD / UOM.MT)
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

  val contractualIncotermCodeField = FieldDetails("Contractual Incoterm Code", FixedPivotParser(referenceData.incotermCodes))
  val contractualLocationCodeField = FieldDetails("Contractual Location Code", FixedPivotParser(referenceData.contractLocationCodes))
  val destinationIncotermCodeField = FieldDetails("Destination Incoterm Code", FixedPivotParser(referenceData.incotermCodes))
  val destinationLocationCodeField = FieldDetails("Destination Location Code", FixedPivotParser(referenceData.countryCodes))
  val extendedKeys = List(contractualIncotermCodeField, contractualLocationCodeField, destinationIncotermCodeField, destinationLocationCodeField)

  val names@List(contractualIncotermField, contractualLocationField, destinationIncotermField, destinationLocationField) =
    List("Contractual Incoterm", "Contractual Location", "Destination Incoterm", "Destination Location").map(FieldDetails(_))

  override def derivedFieldDetails = names

  val values@List(parityRateField, commentField) =
    List(FieldDetails.createMeasure("Parity Rate", parser0 = PivotQuantityPivotParser), FieldDetails.createMeasure("Comment"))

  def valueFieldDetails = values

  override val zeroFields = List(parityRateField.field)
  val initialPivotState = PivotFieldsState(dataFields = valueFields, rowFields = keyFields.toList, columnFields = Nil)

  def createKey(row: Row) = FreightParityDataKey(
    IncotermCode(row.string(contractualIncotermCodeField)), ContractualLocationCode(row.string(contractualLocationCodeField)),
    IncotermCode(row.string(destinationIncotermCodeField)), NeptuneCountryCode(row.string(destinationLocationCodeField)))

  def createValue(rows: List[Row]): dataType = Row.singleRow(rows, "freight parity rate") |> { row =>
    FreightParityData(row.quantity(parityRateField).value, row.string(commentField))
  }

  protected def fieldValuesFor(key: FreightParityDataKey) = Row(
    contractualIncotermField.field → referenceData.incotermFor(key.contractualIncoterm).name,
    contractualLocationField.field → referenceData.contractLocationFor(key.contractualLocation).name,
    destinationIncotermField.field → referenceData.incotermFor(key.destinationIncoterm).name,
    destinationLocationField.field → referenceData.countryFor(key.destinationLocation).name
  )

  def rows(key: FreightParityDataKey, data: FreightParityData) = List(
    fieldValues(key) +
    (contractualIncotermCodeField.field → key.contractualIncoterm.code) +
    (contractualLocationCodeField.field → key.contractualLocation.code) +
    (destinationIncotermCodeField.field → key.destinationIncoterm.code) +
    (destinationLocationCodeField.field → key.destinationLocation.code) +
    (parityRateField.field → data.parityQuantity) +
    (commentField.field → data.comment)
  )

  override val defaultValue = Row(parityRateField.field → Quantity(0, UOM.USD / UOM.MT), commentField.field → "XXDefault")
}

case class ContractualLocationCode(code: String) {
  override def toString = code
}
case class ContractualLocation(code: ContractualLocationCode, name: String) {
  override def toString = name
}