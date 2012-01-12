package starling.marketdata

import starling.pivot._
import pivotparsers.{PeriodPivotParser}
import scalaz.Scalaz._
import starling.quantity.{Quantity, UOM}
import collection.immutable.TreeMap
import starling.daterange.{Year}
import collection.SortedMap
import utils.PeriodPivotFormatter
import starling.utils.ImplicitConversions._
import starling.market._

object FreightFlatRateDataType extends MarketDataType {
  type dataType = FreightFlatRateData
  type keyType = FreightFlatRateDataKey
  val humanName = "Freight flat rates"
  lazy val keys: List[FreightFlatRateDataKey] = Market.allMarketsView.filter(_.commodity == Freight).filter(_.currency == UOM.WSC).map(FreightFlatRateDataKey)
  lazy val marketField = FieldDetails("Market", ValidMarketParserObject.Parser)
  lazy val marketTenorField = FieldDetails("Market Tenor")
  lazy val periodField = FieldDetails("Period", PeriodPivotParser, PeriodPivotFormatter)
  lazy val validity = new FieldDetails("Validity")
  lazy val priceField = new PivotQuantityFieldDetails("Price")

  lazy val initialPivotState = PivotFieldsState(
    filters = List((marketField.field, SomeSelection(Set()))),
    dataFields = List(priceField.field),
    rowFields = List(periodField.field)
  )

  def extendedKeys = List(marketField)

  override def derivedFieldDetails = List(marketTenorField, validity)

  override def valueKeys = List(periodField)

  def valueFieldDetails = List(priceField)

  override def createValue(rows: List[Row]) = {
    val pairs = rows.map {
      row => row[Year](periodField) → row.pivotQuantity(priceField)
    }.toMap

    FreightFlatRateData(pairs)
  }

  override def createKey(row: Row) = FreightFlatRateDataKey(Market.fromName(row.string(marketField)))

  protected def fieldValuesFor(key: FreightFlatRateDataKey) = Row(
    marketField.field → key.market.name,
    marketTenorField.field → key.market.tenor.toString)

  def rows(key: FreightFlatRateDataKey, data: FreightFlatRateData) = data.prices.map {
    case (period, price) => {
      Row(marketField.field → key.market.name,
        marketTenorField.field → key.market.tenor.toString,
        periodField.field → period,
        validity.field → price.warning.isEmpty,
        priceField.field → price)
    }
  }
}

case class FreightFlatRateDataKey(market: CommodityMarket) extends MarketDataKey {

  import FreightFlatRateDataType._

  require(market.priceUOM != null, "missing priceUOM in market: " + market)

  type marketDataType = FreightFlatRateData
  type marketDataDBType = FreightFlatRateDataDTO

  def typeName = FreightFlatRateDataType.name

  def humanName = market.name

  override def unmarshallDB(dbValue: Any): marketDataType =
    FreightFlatRateData.fromSorted(dbValue.asInstanceOf[marketDataDBType].prices, market.priceUOM)

  def fields = Set(marketField, marketTenorField).map(_.field)
}

case class FreightFlatRateDataDTO(prices: SortedMap[Year, Double])

case class FreightFlatRateData(prices: Map[Year, PivotQuantity]) extends MarketData {
  def isEmpty = prices.isEmpty

  def nonEmpty = prices.nonEmpty

  override def marshall = FreightFlatRateDataDTO(TreeMap.empty[Year, Double] ++ prices.mapValues(_.doubleValue.get))

  lazy val sortedKeys = marshall.prices.keySet

  override def size = prices.size
}

object FreightFlatRateData {
  def fromSorted(prices: SortedMap[Year, Double], uom: UOM) = FreightFlatRateData(prices.mapValues(price => Quantity(price, uom).pq).toMap)

  def create(prices: TraversableOnce[(Year, Double)], uom: UOM) = fromMap(prices.toMap, uom)

  def fromMap(prices: Map[Year, Quantity]) = FreightFlatRateData(prices.mapValues(_.pq))

  def fromMap(prices: Map[_ <: Year, Double], uom: UOM) = FreightFlatRateData(prices.asInstanceOf[Map[Year, Double]].mapValues(price => Quantity(price, uom).pq))
}
