package starling.instrument

import starling.daterange.TimeOfDay._
import starling.richdb.RichInstrumentResultSetRow
import cern.jet.random.Normal
import starling.utils.ImplicitConversions._
import starling.models._
import starling.quantity.{UOM, Quantity}
import starling.quantity.Quantity._
import starling.daterange._
import java.lang.String
import starling.utils.Log
import starling.daterange.SpreadPeriod
import starling.market.{KnownExpiry, Market, FuturesMarket}
import starling.curves._

abstract class SpreadOption(
                         market: Market with KnownExpiry,
                         period: Period,
                         strike: Quantity,
                         volume: Quantity,
                         callPut: CallOrPut
                         ) extends Tradeable with MultiLeg {
  def isLive(dayAndTime: DayAndTime) = dayAndTime < expiryDay.get.endOfDay

  def tradeableDetails = Map("Market" -> market, "Period" -> period, "Strike" -> strike, "Quantity" -> volume, "CallPut" -> callPut)
}

abstract class SingleSpreadOption(
                               market: Market with KnownExpiry,
                               exerciseDay: Day,
                               period: Period,
                               strike: Quantity,
                               volume: Quantity,
                               callPut: CallOrPut
                               ) extends UTP {

  def details = Map("Market" -> market, "Period" -> period, "Strike" -> strike, "CallPut" -> callPut)

  def isLive(dayAndTime: DayAndTime) = dayAndTime < exerciseDay.endOfDay

  def assets(env: Environment) = {
    if (isLive(env.marketDay)) {
      val marketDay = env.marketDay.day
      val mtm = price(env) * volume
      Assets(Asset.estimatedCash(env.marketDay.day, mtm, mtm))
    } else {
      Assets()
    }
  }

  def daysForPositionReport(marketDay: DayAndTime): Seq[Day] = List(exerciseDay)

  override def priceAndVolKeys(marketDay: DayAndTime) : (Set[EnvironmentDifferentiable with PriceKey], Set[EnvironmentDifferentiable with VolKey])

  override def riskMarketExtra = String.format("%6.2f%n ", new java.lang.Double(strike.value)) + callPut.toShortString

  override def interpolatedVol(env: Environment, volKey: EnvironmentDifferentiable with VolKey): Quantity

  def price(env: Environment): Quantity
}

