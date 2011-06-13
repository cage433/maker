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
  val Parser = new ValidMarketParser(Market.all.map(_.name.trim.toLowerCase).toSet)
}

object PriceDataType extends MarketDataType {
  type dataType = PriceData
  val keys = Market.all.map(PriceDataKey)
  val exchangeField = FieldDetails("Exchange")
  val marketField = FieldDetails("Market", ValidMarketParserObject.Parser)
  val marketCommodityField = FieldDetails("Market Commodity")
  val marketTenorField = FieldDetails("Market Tenor")
  val periodField = FieldDetails("Period", PeriodPivotParser)
  val validity = new FieldDetails("Validity")
  val priceField = new FieldDetails(Field("Price")) {
    override def parser =  PivotQuantityPivotParser
    override def formatter = PivotQuantitySetPivotFormatter
    override def fixEditedValue(value: Any) = PriceValue(value.asInstanceOf[PivotQuantity].quantityValue.get)
    override def value(a: Any) = {
      a.asInstanceOf[Set[PriceValue]].map(_.pq)
    }
    override def isDataField = true
    override def transformValueForGroupByField(a: Any) = a.asInstanceOf[PriceValue].pq
  }

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(priceField.field),
    rowFields=List(periodField.field)
  )

  val fields = List(exchangeField, marketField, marketCommodityField, marketTenorField, periodField, priceField, validity)
  override def keyFields = Set(marketField, periodField).map(_.field)
  override def valueFields = Set(priceField.field)

  override def createValue(values: List[Map[Field, Any]]) = {
    val pairs = (values.map { v => {
      v(periodField.field).asInstanceOf[DateRange] → v(priceField.field).asInstanceOf[PriceValue]
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
        validity.field → price.validity,
        priceField.field → price).addSome(exchangeField.field → market.safeCast[FuturesMarket].map(_.exchange.name))
  } }

  override def unmarshallDB(dbValue: Any): marketDataType =
    PriceData.fromSorted(dbValue.asInstanceOf[marketDataDBType].prices, market.priceUOM)

  def fieldValues = Map(
    PriceDataType.marketField.field → market.name,
    PriceDataType.marketCommodityField.field → market.commodity.toString,
    PriceDataType.marketTenorField.field → market.tenor.toString)
}

case class PriceDataDTO(prices: SortedMap[DateRange, Double])

case class PriceValue(value:Quantity, warning:Option[String]=None) {
  def <(other: Quantity): Boolean = value < other
  def >(other: Quantity): Boolean = value > other
  def pq = value.pq.copy(warning=warning)
  def validity = if (warning.isEmpty) "Valid" else "Invalid"
}

case class PriceData(prices: Map[DateRange, PriceValue]) extends MarketData {
  def isEmpty = prices.isEmpty
  def nonEmpty = prices.nonEmpty

  override def marshall = PriceDataDTO(TreeMap.empty[DateRange, Double] ++ prices.mapValues(_.value.value))

  lazy val sortedKeys = marshall.prices.keySet
  override def size = Some(prices.size)
}

object PriceData {
  def fromSorted(prices: SortedMap[DateRange, Double], uom: UOM) = PriceData(prices.mapValues(price => PriceValue(Quantity(price, uom))).toMap)
  def create(prices: TraversableOnce[(DateRange, Double)], uom: UOM) = fromMap(prices.toMap, uom)
  def fromMap(prices: Map[DateRange, Quantity]) = PriceData(prices.mapValues(PriceValue(_)))
  def fromMap(prices: Map[DateRange, Double], uom: UOM) = PriceData(prices.mapValues(price => PriceValue(Quantity(price, uom))))
}
