package starling.marketdata

import starling.pivot._
import scalaz.Scalaz._
import starling.quantity.{Quantity, UOM}

case class FreightParityData(parityRate: Double, comment: String) extends MarketData {
  def size = 1
}

case class ContractualLocationCode(code: String)
case class ContractualLocation(code: ContractualLocationCode, name: String) {
  override def toString = name
}

case class FreightParityDataKey(contractualIncoterm: IncotermCode, contractualLocation: ContractualLocationCode,
  destinationIncoterm: IncotermCode, destinationLocation: NeptuneCountryCode) extends MarketDataKey {

  import FreightParityDataType._

  type marketDataType = FreightParityData
  type marketDataDBType = FreightParityData
  def dataType = FreightParityDataType
  def subTypeKey = ""
  def fieldValues = Map(contractualIncotermField.field → contractualIncoterm.code,
    contractualLocationField.field → contractualLocation.code, destinationIncotermField.field → destinationIncoterm.code,
    destinationLocationField.field → destinationLocation.code)

  override def rows(marketData: FreightParityData, referenceDataLookup: ReferenceDataLookup): List[Row] = List(Row(
    contractualIncotermField.field → referenceDataLookup.incotermFor(contractualIncoterm),
    contractualLocationField.field → referenceDataLookup.contractLocationFor(contractualLocation),
    destinationIncotermField.field → referenceDataLookup.incotermFor(destinationIncoterm),
    destinationLocationField.field → referenceDataLookup.countryFor(destinationLocation),
    parityRateField.field → Quantity(marketData.parityRate, UOM.USD / UOM.MT),
    commentField.field → marketData.comment
  ))
}

object FreightParityDataType extends MarketDataType {
  type dataType = FreightParityData
  override val readonly = true

  val keys@List(contractualIncotermField, contractualLocationField, destinationIncotermField, destinationLocationField) =
    List("Contractual Inco Terms", "Contractual Location", "Destination Inco Terms", "Destination Location").map(FieldDetails(_))

  val values@List(parityRateField, commentField) =
    List(FieldDetails.createMeasure("Parity Rate"), FieldDetails.createMeasure("Comment"))

  val fields = keys ::: values
  override val keyFields = keys.map(_.field).toSet
  override val valueFields = values.map(_.field)
  val marketDataKeyFields = keyFields
  override val zeroFields = List(parityRateField.field)
  val initialPivotState = PivotFieldsState(dataFields = values.map(_.field), rowFields = keys.map(_.field), columnFields = Nil)

  def createKey(row: Row) = FreightParityDataKey(
    row[Incoterm](contractualIncotermField).code, row[ContractualLocation](contractualLocationField).code,
    row[Incoterm](destinationIncotermField).code, row[NeptuneCountry](destinationLocationField).code)

  def createValue(rows: List[Row]): dataType = Row.singleRow(rows, "freight parity rate") |> { row =>
    FreightParityData(row.quantity(parityRateField).value, row.string(commentField))
  }
}