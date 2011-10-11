package starling.marketdata

import starling.pivot._
import starling.market._
import starling.daterange.StoredFixingPeriod
import starling.quantity.{Quantity, Percentage}
import scalaz.Scalaz._

object PriceFixingsHistoryDataType extends MarketDataType {
  type dataType = PriceFixingsHistoryData
  type keyType = PriceFixingsHistoryDataKey
  val marketField = FieldDetails("Market")
  val levelField = FieldDetails("Level")
  val periodField = new FieldDetails("Period") {
    override def comparator = StoredFixingPeriod.Comparator
  }
  val priceField = new MarketValueFieldDetails("Price")
  val exchangeField = FieldDetails("Exchange")

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field, SomeSelection(Set()))),
    dataFields=List(priceField.field),
    rowFields=List(levelField.field, periodField.field)
  )

  val fields = List(exchangeField, marketField, levelField, periodField, priceField)

  def marketDataKeyFields = Set(marketField.field)
  def keyFields = Set(exchangeField.field, marketField.field, levelField.field, periodField.field)
  def valueFields = List(priceField.field)
  def createKey(row: Row) = PriceFixingsHistoryDataKey(row.string(marketField), row[Option[String]](exchangeField))

  def createValue(rows: List[Row]) = {
    val prices = rows.map { row =>
      (Level.fromName(row.string(levelField)), StoredFixingPeriod.parse(row(periodField))) → marketValue(row(priceField))
    }

    PriceFixingsHistoryData.create(prices)
  }


  def rows(key: PriceFixingsHistoryDataKey, data: PriceFixingsHistoryData, referenceDataLookup: ReferenceDataLookup) = {
    data.fixings.map { case ((level, period), fixing) =>
      Row(
        PriceFixingsHistoryDataType.marketField.field → key.marketName,
        PriceFixingsHistoryDataType.levelField.field → level.name,
        PriceFixingsHistoryDataType.periodField.field → period,
        PriceFixingsHistoryDataType.priceField.field → fixing.pivotValue
      ) +? (PriceFixingsHistoryDataType.exchangeField.field → key.exchangeName)
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



