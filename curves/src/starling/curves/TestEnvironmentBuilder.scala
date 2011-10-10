package starling.curves

import starling.quantity.{Percentage, Quantity, UOM}
import starling.quantity.UOM._
import starling.daterange.{DayAndTime, Day, DateRange}
import starling.utils.CollectionUtils
import starling.quantity.Percentage._
import starling.marketdata.MarketData
import starling.market._
import collection.immutable.TreeMap

case class TestEnvironmentBuilder(marketDay : DayAndTime) {  builder =>
	var curves = Map.empty[CurveKey, CurveObject]
	var spotUsdRates = Map.empty[UOM, Quantity]
 
	def setConstantPrice(market : CommodityMarket, price : Quantity){
	  curves += ForwardCurveKey(market) -> ConstantForwardCurve(marketDay, market, price)
	}
	def setConstantPrice(market : CommodityMarket, price : Double){
	  setConstantPrice(market, Quantity(price, market.priceUOM))
	}
	def setConstantPrice(market : CommodityMarket, days : List[Day], price : Quantity){
	  setPrices(market, days, List.fill(days.size)(price.value))
	}

	def setPrices(market : CommodityMarket, days : List[Day], prices : List[Double]){
	  curves += 
     ForwardCurveKey(market) -> 
     	ForwardCurve.create(
     			market, marketDay, days.zip(prices).toMap
     	)
	}

  def setImpliedVol(market : CommodityMarket, vol : Double) {
    setImpliedVol(market, Percentage(vol))
  }

  def setAsianVol(market : CommodityMarket, vol : Double) {
    setAsianVol(market, Percentage(vol))
  }

	def setZeroRate(ccy : UOM, z : Double){
	  curves += DiscountCurveKey(ccy) -> ConstantDiscountCurve(marketDay, ccy, z)
	}
 
	def setUSDPerCCYSpotRate(ccy : UOM, rate : Double){
	  spotUsdRates += ccy -> Quantity(rate, USD / ccy)
	}
	def setUSDPerCCYSpotRate(ccy : UOM, rate : Quantity){
	  setUSDPerCCYSpotRate(ccy, rate.checkedValue(USD / ccy))
	}
//  def setHistoricFixings(index : TrinityIndex, fixingDays : List[Day], prices : List[Double]){
//    val fixings = Map.empty ++ fixingDays.zip(prices).map{case (d, price) => (d -> (d, Quantity(price, index.market.priceUOM)))}
//    curves += FixingsHistoryKey(index) -> FixingsHistory(marketDay, index, fixings)
//  }
	def curveObjectEnvironment() : CurveObjectEnvironment = MappingCurveObjectEnvironment(curves, marketDay)
	def build : Environment = {
    spotUsdRates.foreach{
      case (ccy, rate) =>
        curves += USDFXRateCurveKey(ccy) -> USDFXRate(marketDay, rate)
    }
	  Environment(curveObjectEnvironment())
	}
 
}

object TestEnvironmentBuilder {
  def curveObjectEnvironment(
          dayAndTime:DayAndTime,
          market:CommodityMarket,
          price:Double = 10.0,
          interestRate:Double = 0.0,
          vol:Double = 0.20) = {
    val builder = new TestEnvironmentBuilder(dayAndTime)
    builder.setConstantPrice(market, price)
    builder.setZeroRate(market.currency, interestRate)
    builder.setImpliedVol(market, vol)
    builder.curveObjectEnvironment
  }
}