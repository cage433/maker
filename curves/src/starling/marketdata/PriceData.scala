package starling.marketdata

import collection.SortedMap
import starling.pivot._
import starling.pivot.pivotparsers.{PeriodPivotParser}
import starling.utils.ImplicitConversions._
import starling.quantity.{UOM, Quantity}
import collection.immutable.TreeMap
import starling.market.{FuturesMarket, Market, CommodityMarket}
import utils.PeriodPivotFormatter
import starling.daterange._
import scalaz.Scalaz._

object ValidMarketParserObject {
  lazy val Parser = new SpecifiedValuesParser(Market.allMarketsView.map(_.name).toSet)
}

object PeriodComparator extends Ordering[Any] {
  def compare(x: Any, y: Any) = {
    (x,y) match {
      case (p1:Period, p2:Period) => {
        (p1, p2) match {
          case (_: StripPeriod, _: DateRangePeriod) => -1
          case (_: DateRangePeriod, _: StripPeriod) => +1

          case (_: DateRangePeriod, _: SpreadPeriod) => +1
          case (_: SpreadPeriod, _: DateRangePeriod) => -1

          case (_: StripPeriod, _: SpreadPeriod) => -1
          case (_: SpreadPeriod, _: StripPeriod) => +1

          case (a:SpreadPeriod, b:SpreadPeriod) => a.compare(b)
          case (a:DateRangePeriod, b:DateRangePeriod) => a.compare(b)
          case (a:StripPeriod, b:StripPeriod) => a.compare(b)
        }
      }
    }
  }
}

class PeriodFieldDetails(name:String) extends FieldDetails(name) {
  override def parser = PeriodPivotParser
  override def formatter = PeriodPivotFormatter
  override def comparator:Ordering[Any] = PeriodComparator
}
object PeriodFieldDetails {
  def apply(name:String) = new PeriodFieldDetails(name)
}

object PriceDataType extends MarketDataType {
  type dataType = PriceData
  type keyType = PriceDataKey
  val humanName = "prices"
  lazy val keys = Market.allMarketsView.map(PriceDataKey)
  lazy val exchangeField = FieldDetails("Exchange")
  lazy val marketField = FieldDetails("Market", ValidMarketParserObject.Parser)
  lazy val marketCommodityField = FieldDetails("Market Commodity")
  lazy val marketTenorField = FieldDetails("Market Tenor")
  lazy val periodField = FieldDetails("Period", PeriodPivotParser, PeriodPivotFormatter)
  lazy val validity = new FieldDetails("Validity")
  lazy val priceField = new PivotQuantityFieldDetails("Price")

  lazy val initialPivotState = PivotFieldsState(
    filters=List(exchangeField.field->AllSelection),
    rowFields=List(marketField.field, Field("Observation Time"), periodField.field),
    dataFields=List(priceField.field)
  )

  def extendedKeys = List(marketField)
  override def derivedFieldDetails = List(exchangeField, marketTenorField, validity)
  override def valueKeys = List(periodField)
  def valueFieldDetails = List(priceField)

  override def createValue(rows: List[Row]) = {
    val pairs = rows.map { row => row[DateRange](periodField) → row.pivotQuantity(priceField) }.toMap

    PriceData(pairs)
  }

  override def createKey(row: Row) = PriceDataKey(Market.fromName(row.string(marketField)))

  protected def fieldValuesFor(key: PriceDataKey) = Row(
    marketField.field → key.market.name,
    marketCommodityField.field → key.market.commodity.toString,
    marketTenorField.field → key.market.tenor.toString) +?
    (exchangeField.field → key.market.safeCast[FuturesMarket].map(_.exchange.name))

  def rows(key: PriceDataKey, data: PriceData) = data.prices.map { case (period, price) => {
    Row(marketField.field → key.market.name,
      marketCommodityField.field → key.market.commodity.toString,
      marketTenorField.field → key.market.tenor.toString,
      periodField.field → period,
      validity.field → price.warning.isEmpty,
      priceField.field → price) +? (exchangeField.field → key.market.safeCast[FuturesMarket].map(_.exchange.name))
  } }
}

case class PriceDataKey(market: CommodityMarket) extends MarketDataKey {
  import PriceDataType._
  require(market.priceUOM != null, "missing priceUOM in market: " + market)

  type marketDataType = PriceData
  type marketDataDBType = PriceDataDTO
  def typeName = PriceDataType.name
  def humanName = market.name

  def fields = market.safeCast[FuturesMarket].fold(_ => Set(exchangeField.field), Set.empty[Field]) ++
    Set(marketField, marketCommodityField, marketTenorField).map(_.field)
}

case class PriceDataDTO(prices: SortedMap[DateRange, Double])

case class PriceData(prices: Map[DateRange, PivotQuantity]) extends MarketData {
  def isEmpty = prices.isEmpty
  def nonEmpty = prices.nonEmpty

  override def marshall = PriceDataDTO(TreeMap.empty[DateRange, Double] ++ prices.mapValues(_.doubleValue.get))

  lazy val sortedKeys = marshall.prices.keySet
  override def size = prices.size
}

object PriceData {
  def fromSorted(prices: SortedMap[DateRange, Double], uom: UOM) = PriceData(prices.mapValues(price => Quantity(price, uom).pq).toMap)
  def create(prices: TraversableOnce[(DateRange, Double)], uom: UOM) = fromMap(prices.toMap, uom)
  def fromMap(prices: Map[DateRange, Quantity]) = PriceData(prices.mapValues(_.pq))
  def fromMap(prices: Map[_ <: DateRange, Double], uom: UOM) = PriceData(prices.asInstanceOf[Map[DateRange, Double]].mapValues(price => Quantity(price, uom).pq))
}
