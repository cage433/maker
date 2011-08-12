package starling.marketdata

import starling.market._
import starling.utils.ImplicitConversions._
import starling.quantity.{UOMSymbol, UOM}
import starling.utils.Log

case class PriceFixingsHistoryDataKey(marketName: String, exchangeName: Option[String] = None) extends MarketDataKey {
  type marketDataType = PriceFixingsHistoryData
  type marketDataDBType = PriceFixingsHistoryData
  def dataType = PriceFixingsHistoryDataType
  def subTypeKey = marketName

  override def rows(data : PriceFixingsHistoryData) = {
    data.fixings.map { case ((level, period), fixing) =>
      Map(
        PriceFixingsHistoryDataType.marketField.field → marketName,
        PriceFixingsHistoryDataType.levelField.field → level.name,
        PriceFixingsHistoryDataType.periodField.field → period,
        PriceFixingsHistoryDataType.priceField.field → fixing.pivotValue
      ).addSome(PriceFixingsHistoryDataType.exchangeField.field → exchangeName)
    }
  }

  def fieldValues = Map(PriceFixingsHistoryDataType.marketField.field → marketName)
}

object PriceFixingsHistoryDataKey {
  val currencyNames = UOMSymbol.currencySymbols.map(_.name)

  def apply(market: CommodityMarket): PriceFixingsHistoryDataKey =
    PriceFixingsHistoryDataKey(tradeableNameOf(market), exchangeOf(market))

  private def exchangeOf(market: CommodityMarket): Option[String] = market partialMatch {
    case publishedIndex:PublishedIndex if publishedIndex.businessCalendar.name == "IcS" => "BALTIC"
    case futuresMarket: FuturesMarket => futuresMarket.exchange.name
  }

  private def tradeableNameOf(market: CommodityMarket): String = market match {
      // TODO I don't understand the logic here but I know it doesn't work for Oil markets so I'll restrict it to metal
    case futuresMarket: FuturesMarket if futuresMarket.commodity.isInstanceOf[MetalCommodity] => futuresMarket.commodity.name
    case _ => market.name
  }
}

