package starling.marketdata

import starling.market._
import starling.quantity.UOM
import starling.utils.ImplicitConversions._
import starling.pivot.Row


case class PriceFixingsHistoryDataKey(marketName: String, exchangeName: Option[String] = None) extends MarketDataKey {
  type marketDataType = PriceFixingsHistoryData
  def typeName = PriceFixingsHistoryDataType.name
  def humanName = exchangeName.map(e => if (marketName.startsWith(e)) marketName else { e + " " + marketName}).getOrElse(marketName)
  def fields = Set(PriceFixingsHistoryDataType.marketField.field)
}

object PriceFixingsHistoryDataKey {
  val currencyNames = UOM.currencies.map(_.identifier)

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