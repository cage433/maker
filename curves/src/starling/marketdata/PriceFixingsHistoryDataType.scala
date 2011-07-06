package starling.marketdata

import starling.pivot._
import starling.market._
import starling.daterange.StoredFixingPeriod
import starling.quantity.{Quantity, Percentage}

object PriceFixingsHistoryDataType extends MarketDataType {
  type dataType = PriceFixingsHistoryData
  val marketField = FieldDetails("Market")
  val levelField = FieldDetails("Level")
  val periodField = FieldDetails("Period")
  val priceField = new MarketValueFieldDetails("Price")
  val exchangeField = FieldDetails("Exchange")

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field, SomeSelection(Set()))),
    dataFields=List(priceField.field),
    rowFields=List(levelField.field, periodField.field)
  )

  val fields = List(exchangeField, marketField, levelField, periodField, priceField)

  def keyFields = Set(exchangeField.field, marketField.field, levelField.field, periodField.field)
  def valueFields = Set(priceField.field)
  def createKey(values: Map[Field, Any]) = PriceFixingsHistoryDataKey(
    Market.fromNameOrCommodity(values(exchangeField.field).asInstanceOf[String], values(marketField.field).asInstanceOf[String]))

  def createValue(values: List[Map[Field, Any]]) = {
    val prices: List[((Level, StoredFixingPeriod), MarketValue)] = values.map { row =>
      ((Level.fromName(row(levelField.field).asInstanceOf[String]), row(periodField.field).asInstanceOf[StoredFixingPeriod]),
        marketValue(row(priceField.field)))
    }
    PriceFixingsHistoryData.create(prices)
  }

  private def marketValue(value: Any) = value match {
    case pq: PivotQuantity => MarketValue.quantity(pq.quantityValue.get)
    case p: Percentage => MarketValue.percentage(p)
    case s: String => MarketValue.fromString(s)
    case mv: MarketValue => mv
    case _ => throw new Exception("Unknown type: " + value)
  }
}



