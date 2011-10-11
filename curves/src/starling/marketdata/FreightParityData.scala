package starling.marketdata

import starling.pivot._
import scalaz.Scalaz._
import starling.quantity.{Quantity, UOM}
import starling.pivot.Row._

case class FreightParityData(parityRate: Double, comment: String) extends MarketData {
  def size = 1
}

case class FreightParityDataKey(contractualIncoterm: IncotermCode, contractualLocation: ContractualLocationCode,
  destinationIncoterm: IncotermCode, destinationLocation: NeptuneCountryCode) extends MarketDataKey {

  import FreightParityDataType._

  type marketDataType = FreightParityData
  type marketDataDBType = FreightParityData
  def dataType = FreightParityDataType
  def subTypeKey = ""
  def fieldValues(referenceDataLookup: ReferenceDataLookup) = Row(
    contractualIncotermField.field → referenceDataLookup.incotermFor(contractualIncoterm).name,
    contractualLocationField.field → referenceDataLookup.contractLocationFor(contractualLocation).name,
    destinationIncotermField.field → referenceDataLookup.incotermFor(destinationIncoterm).name,
    destinationLocationField.field → referenceDataLookup.countryFor(destinationLocation).name
  )
}

object FreightParityDataType extends MarketDataType {
  type dataType = FreightParityData
  type keyType = FreightParityDataKey

  val keys@List(contractualIncotermCodeField, contractualLocationCodeField, destinationIncotermCodeField, destinationLocationCodeField) =
    List("Contractual Incoterm Code", "Contractual Location Code", "Destination Incoterm Code", "Destination Location Code").map(FieldDetails(_))

  val names@List(contractualIncotermField, contractualLocationField, destinationIncotermField, destinationLocationField) =
    List("Contractual Incoterm", "Contractual Location", "Destination Incoterm", "Destination Location").map(FieldDetails(_))

  val values@List(parityRateField, commentField) =
    List(FieldDetails.createMeasure("Parity Rate", parser0 = PivotQuantityPivotParser), FieldDetails.createMeasure("Comment"))

  val fields = keys ::: names ::: values
  override val keyFields = keys.map(_.field).toSet
  override val valueFields = values.map(_.field)
  val marketDataKeyFields = keyFields
  override val zeroFields = List(parityRateField.field)
  val initialPivotState = PivotFieldsState(dataFields = values.map(_.field), rowFields = names.map(_.field), columnFields = Nil)

  def createKey(row: Row) = FreightParityDataKey(
    IncotermCode(row.string(contractualIncotermCodeField)), ContractualLocationCode(row.string(contractualLocationCodeField)),
    IncotermCode(row.string(destinationIncotermCodeField)), NeptuneCountryCode(row.string(destinationLocationCodeField)))

  def createValue(rows: List[Row]): dataType = Row.singleRow(rows, "freight parity rate") |> { row =>
    FreightParityData(row.quantity(parityRateField).value, row.string(commentField))
  }

  def rows(key: FreightParityDataKey, data: FreightParityData, referenceDataLookup: ReferenceDataLookup) =  List(
    key.fieldValues(referenceDataLookup) +
    (contractualIncotermCodeField.field → key.contractualIncoterm.code) +
    (contractualLocationCodeField.field → key.contractualLocation.code) +
    (destinationIncotermCodeField.field → key.destinationIncoterm.code) +
    (destinationLocationCodeField.field → key.destinationLocation.code) +
    (parityRateField.field → Quantity(data.parityRate, UOM.USD / UOM.MT)) +
    (commentField.field → data.comment)
  )
}

case class ContractualLocationCode(code: String)
case class ContractualLocation(code: ContractualLocationCode, name: String) {
  override def toString = name
}