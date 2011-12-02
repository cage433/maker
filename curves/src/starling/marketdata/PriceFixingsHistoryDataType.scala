package starling.marketdata

import starling.pivot._
import starling.market._
import starling.daterange.StoredFixingPeriod
import starling.quantity.{Quantity, Percentage}
import scalaz.Scalaz._

object PriceFixingsHistoryDataType extends MarketDataType {
  type dataType = PriceFixingsHistoryData
  type keyType = PriceFixingsHistoryDataKey
  val humanName = "fixings"
  val marketField = FieldDetails("Market")
  val levelField = FieldDetails.list("Level", Level.names)
  val periodField = new FieldDetails("Period") {
    override def comparator = StoredFixingPeriod.Comparator
  }
  val priceField = new MarketValueFieldDetails("Price")
  val exchangeField = FieldDetails("Exchange")

  val initialPivotState = PivotFieldsState(
    dataFields=List(priceField.field),
    rowFields=List(exchangeField.field, marketField.field, Field("Observation Time"), levelField.field, periodField.field)
  )

  def extendedKeys = List(exchangeField, marketField)
  override def valueKeys = List(levelField, periodField)
  def valueFieldDetails = List(priceField)

  def createKey(row: Row) = PriceFixingsHistoryDataKey(row.string(marketField), row.get[String](exchangeField))

  def createValue(rows: List[Row]) = {
    val prices = rows.map { row =>
      (Level.fromName(row.string(levelField)), StoredFixingPeriod.parse(row(periodField))) → marketValue(row(priceField))
    }

    PriceFixingsHistoryData.create(prices)
  }

  protected def fieldValuesFor(key: PriceFixingsHistoryDataKey) = Row(marketField.field → key.marketName) +?
    (exchangeField.field → key.exchangeName)

  def rows(key: PriceFixingsHistoryDataKey, data: PriceFixingsHistoryData) = {
    data.fixings.map { case ((level, period), fixing) =>
      Row(
        marketField.field → key.marketName,
        levelField.field → level.name,
        periodField.field → period,
        priceField.field → fixing.pivotValue
      ) +? (exchangeField.field → key.exchangeName)
    }
  }

  private def marketValue(value: Any) = value match {
    case pq: PivotQuantity => MarketValue.quantity(pq.quantityValue.get)
    case p: Percentage => MarketValue.percentage(p)
    case q: Quantity => MarketValue.quantity(q)
    case s: String => MarketValue.fromString(s)
    case mv: MarketValue => mv
    case _ => throw new Exception("Unknown type: " + value)
  }
}



