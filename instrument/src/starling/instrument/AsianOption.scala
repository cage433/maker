package starling.instrument


import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.Quantity._
import java.sql.ResultSet
import starling.utils.CollectionUtils._
import starling.utils.ImplicitConversions._
import starling.daterange._
import starling.quantity.Percentage._
import starling.curves._
import starling.models.{Put, BlackScholes, CallOrPut}
import starling.market._
import starling.quantity.{NamedQuantity, Percentage, UOM, Quantity}
import starling.quantity.SimpleNamedQuantity
import starling.quantity.FunctionNamedQuantity


case class AsianOption(
  index : SingleIndex,
  averagingPeriod : Period,
  strike : Quantity,
  volume : Quantity,
  callPut : CallOrPut
)
extends AverageOption(index, averagingPeriod, strike, volume, callPut) with MultiLeg {
  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio({
    supPeriodOptions.map{s => s.copy(volume = s.volume.copy(value = 1.0)) -> s.volume.value}
  }.toMap)

  def supPeriodOptions = periods.map(SingleAsianOption(index, _, strike, volume, callPut))

  def explanation(env : Environment) : NamedQuantity = {
    val subExplanations : List[NamedQuantity] = supPeriodOptions.map{option =>
      val optionExp = option.explanation(env)
      SimpleNamedQuantity(option.averagingPeriod.toShortString + " value", optionExp)
    }
    FunctionNamedQuantity("Sum", subExplanations, subExplanations.map(_.quantity).sum)
  }

  def isLive(dayAndTime: DayAndTime) = dayAndTime < periods.last.lastDay.endOfDay

  override def expiryDay() = Some(index.observationDays(periods.last).last)

  def tradeableType =  AsianOption

  def legs = periods.map(p => AsianOption(index, p, strike, volume, callPut))

  override def fixUpMyCashInstruments(ci: CashInstrument) = {
    ci.copy(index = Some(Left(index)), averagingPeriod = Some(averagingPeriod))
  }
}


case class SingleAsianOption(
  index : SingleIndex,
  override val averagingPeriod : DateRange,
  strike : Quantity,
  volume : Quantity,
  callPut : CallOrPut
) extends SingleAverageOption(index, averagingPeriod, strike, volume, callPut) {

  val averagingDays: List[Day] = index.observationDays(averagingPeriod)

  def instrumentType =  AsianOption

  /**
   * Jon uses the last day in the averaging period as the settlement day. Until we know any better
   * we might as well copy this.
   */
  val settlementDate = averagingDays.sorted.last

  def * (scale : Double) = copy(volume = volume * scale)
  override def riskMarketExtra = String.format("%6.2f%n ", new java.lang.Double(strike.value)) + callPut.toShortString
  override def atomicKeyCachingUTP : UTP = copy(strike = 1.0(index.priceUOM))
}

object AsianOption extends InstrumentType[SingleAsianOption] with TradeableType[AsianOption] {
  val name = "Asian Option"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val index = row.getSingleIndexFromName("Market")
    AsianOption(index, row.getPeriod("Period"),
      row.getQuantity("Strike"), row.getQuantity("Quantity"), row.getCallPut("CallPut"))
  }
  def sample = {
    import starling.quantity.Quantity._
    import starling.quantity.UOM._
    AsianOption(Index.DATED_BRENT, Month(2009, 2), 98(USD/MT), 222(MT), Put)
  }

}
