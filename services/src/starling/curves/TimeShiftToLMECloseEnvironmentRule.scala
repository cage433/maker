package starling.curves

import starling.daterange._
import starling.utils.ImplicitConversions._
import ObservationTimeOfDay._
import collection.immutable.List
import starling.gui.api.EnvironmentRuleLabel
import starling.db.{MarketDataReaderMarketDataSlice, MarketDataReader}
import starling.quantity.Quantity
import starling.market.{FuturesExchangeFactory, Market, FuturesMarket}
import starling.marketdata._

object TimeShiftToLMECloseEnvironmentRule extends EnvironmentRule {
  val observationTimeOfDay = ObservationTimeOfDay.LMEClose
  val label = EnvironmentRuleLabel("Time shift to " + observationTimeOfDay.name)

  def createEnv(observationDay: Day, reader: MarketDataReader) = {
    val pricesForLastClose = Market.futuresMarkets.filter(_.exchange != FuturesExchangeFactory.COMEX).flatMap { market => {
      val closeDay = market.businessCalendar.thisOrPreviousBusinessDay(observationDay)
      try {
        val priceDataAtLastClose = reader.read(ObservationPoint(closeDay, market.closeTime), PriceDataKey(market)).asInstanceOf[PriceData]
        List( market -> (closeDay, priceDataAtLastClose) )
      } catch {
        case e:MissingMarketDataException => Nil
      }
    } }.toMap

    val needsShift = pricesForLastClose.filter { case (market,(closeDay,_)) => {
      closeDay != observationDay || observationTimeOfDay != market.closeTime
    }}

    def previousEnv(day:Day, timeOfDay:ObservationTimeOfDay) = {
      val slice = new MarketDataReaderMarketDataSlice(reader, ObservationPoint(day, timeOfDay))
      Environment(new NamingAtomicEnvironment(new MarketDataCurveObjectEnvironment(day.endOfDay, slice), timeOfDay.name))
    }

    val env = {
      val envDayAndTime = observationDay.endOfDay
      val slice = new MarketDataReaderMarketDataSlice(reader, ObservationPoint(observationDay, observationTimeOfDay))
      new MarketDataCurveObjectEnvironment(envDayAndTime, slice) {
        override def curve(curveKey: CurveKey) = {
          curveKey match {
            case ForwardCurveKey(market:FuturesMarket) if (needsShift.contains(market)) => {
              val shiftMarket = FuturesExchangeFactory.LME.markets.find(_.commodity == market.commodity).get
              val (closeDay, closePrices) = pricesForLastClose(market)

              val lastCloseEnv = previousEnv(closeDay, market.closeTime)
              val currentEnv = previousEnv(observationDay, observationTimeOfDay)
              val priceUOM = shiftMarket.currency / market.uom
              val shiftInShiftingMarketCurrency = {
                currentEnv.forwardPrice(shiftMarket, shiftMarket.frontPeriod(observationDay), priceUOM) -
                  lastCloseEnv.forwardPrice(shiftMarket, shiftMarket.frontPeriod(closeDay))
              }

              val shiftedPrices = closePrices.prices.map { case (period,price) => {
                val shiftedPriceInShiftMarketCurrency = lastCloseEnv.forwardPrice(market, period, priceUOM) + shiftInShiftingMarketCurrency
                val shiftedPrice = shiftedPriceInShiftMarketCurrency / currentEnv.spotFXRate(shiftMarket.currency, market.currency)
                period -> shiftedPrice
              }}
              new ForwardCurve(market, envDayAndTime, shiftedPrices)
            }
            case _ => super.curve(curveKey)
          }
        }
      }
    }

    val marketsX = pricesForLastClose.map { case (market, (closeDay, priceData)) => {
      MarketDeliveryPeriods(observationTimeOfDay, market, priceData.sortedKeys)
    } }

    new EnvironmentWithDomain {
      def environment = Environment(env)
      def markets = marketsX.toList
      override def discounts = reader.readAll(ForwardRateDataType, observationDay.atTimeOfDay(ObservationTimeOfDay.Default)).map {
        case (key:ForwardRateDataKey, data:ForwardRateData) => key.ccy -> data
      }

    }
  }
}