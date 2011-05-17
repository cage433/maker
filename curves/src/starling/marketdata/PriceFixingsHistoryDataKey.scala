package starling.marketdata

import starling.market._
import starling.utils.ImplicitConversions._


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
  def apply(market: CommodityMarket): PriceFixingsHistoryDataKey =
    PriceFixingsHistoryDataKey(tradeableNameOf(market), exchangeOf(market))

  private def exchangeOf(market: CommodityMarket): Option[String] = market partialMatch {
    case futuresMarket: FuturesMarket => futuresMarket.exchange.name
  }

  private def tradeableNameOf(market: CommodityMarket): String = market match {
    case futuresMarket: FuturesMarket => futuresMarket.commodity.name
    case _ => market.name
  }
}

