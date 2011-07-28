package starling.marketdata

import collection.SortedMap
import starling.pivot._
import starling.pivot.pivotparsers.{ValidMarketParser, PeriodPivotParser}
import starling.daterange.DateRange
import starling.utils.ImplicitConversions._
import starling.quantity.{UOM, Quantity}
import collection.immutable.TreeMap
import starling.market.{FuturesMarket, Market, CommodityMarket}


object ValidMarketParserObject {
  lazy val Parser = new ValidMarketParser(Market.all.map(_.name).toSet)
}

object PriceDataType extends MarketDataType {
  type dataType = PriceData
  lazy val keys = Market.all.map(PriceDataKey)
  lazy val exchangeField = FieldDetails("Exchange")
  lazy val marketField = FieldDetails("Market", ValidMarketParserObject.Parser)
  lazy val marketCommodityField = FieldDetails("Market Commodity")
  lazy val marketTenorField = FieldDetails("Market Tenor")
  lazy val periodField = FieldDetails("Period", PeriodPivotParser)
  lazy val validity = new FieldDetails("Validity")
  lazy val priceField = new FieldDetails(Field("Price")) {
    override def parser =  PivotQuantityPivotParser
    override def formatter = PivotQuantitySetPivotFormatter
    override def isDataField = true
  }

  lazy val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(priceField.field),
    rowFields=List(periodField.field)
  )

  lazy val fields = List(exchangeField, marketField, marketCommodityField, marketTenorField, periodField, priceField, validity)
  def marketDataKeyFelds = Set(marketField.field)
  override def keyFields = Set(marketField, periodField).map(_.field)
  override def valueFields = Set(priceField.field)

  override def createValue(values: List[Map[Field, Any]]) = {
    val pairs = (values.map { v => {
      v(periodField.field).asInstanceOf[DateRange] → v(priceField.field).asInstanceOf[PivotQuantity]
    } }).toMap
    PriceData(pairs)
  }

  override def createKey(values: Map[Field, Any]) = PriceDataKey(Market.fromName(values(marketField.field).asInstanceOf[String]))
}

case class PriceDataKey(market: CommodityMarket) extends MarketDataKey {
  import PriceDataType._
  require(market.priceUOM != null, "missing priceUOM in market: " + market)

  type marketDataType = PriceData
  type marketDataDBType = PriceDataDTO
  def dataType = PriceDataType
  def subTypeKey = market.name

  override def rows(data : PriceData) = data.prices.map { case (period, price) => {
    Map(marketField.field → market.name,
        marketCommodityField.field → market.commodity.toString,
        marketTenorField.field → market.tenor.toString,
        periodField.field → period,
        validity.field → price.warning.isEmpty,
        priceField.field → price).addSome(exchangeField.field → market.safeCast[FuturesMarket].map(_.exchange.name))
  } }

  override def unmarshallDB(dbValue: Any): marketDataType =
    PriceData.fromSorted(dbValue.asInstanceOf[marketDataDBType].prices, market.priceUOM)

  def fieldValues = Map(
    PriceDataType.marketField.field → market.name,
    PriceDataType.marketCommodityField.field → market.commodity.toString,
    PriceDataType.marketTenorField.field → market.tenor.toString).addSome(
      exchangeField.field → market.safeCast[FuturesMarket].map(_.exchange.name))
}

case class PriceDataDTO(prices: SortedMap[DateRange, Double])

case class PriceData(prices: Map[DateRange, PivotQuantity]) extends MarketData {
  def isEmpty = prices.isEmpty
  def nonEmpty = prices.nonEmpty

  override def marshall = PriceDataDTO(TreeMap.empty[DateRange, Double] ++ prices.mapValues(_.doubleValue.get))

  lazy val sortedKeys = marshall.prices.keySet
  override def size = Some(prices.size)
}

object PriceData {
  def fromSorted(prices: SortedMap[DateRange, Double], uom: UOM) = PriceData(prices.mapValues(price => Quantity(price, uom).pq).toMap)
  def create(prices: TraversableOnce[(DateRange, Double)], uom: UOM) = fromMap(prices.toMap, uom)
  def fromMap(prices: Map[DateRange, Quantity]) = PriceData(prices.mapValues(_.pq))
  def fromMap(prices: Map[DateRange, Double], uom: UOM) = PriceData(prices.mapValues(price => Quantity(price, uom).pq))
}
