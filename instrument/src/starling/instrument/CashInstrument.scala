package starling.instrument

import java.sql.ResultSet
import starling.market.{Market, FXMarket}
import starling.quantity.{Quantity, UOM, Percentage}
import starling.quantity.UOM._
import starling.richdb.RichInstrumentResultSetRow
import starling.curves._
import starling.market.Index
import starling.daterange._

case class CashInstrument(
  cashInstrumentType: CashInstrumentType,
  volume : Quantity,
  settlementDate : Day,
  index : Option[Either[Index,Market]] = None,
  averagingPeriod : Option[Period] = None
)
	extends UTP 
{
  def this(amount : Quantity, settlementDate : Day) = this(CashInstrumentType.General, amount, settlementDate)

  def *(x : Double) = copy(volume = volume * x)

  def assets(env : Environment) = Assets(Asset.knownCash(settlementDate, volume, env))
  
	def valuationCCY : UOM = volume.uom
	def isLive(dayAndTime : DayAndTime) : Boolean = dayAndTime <= settlementDate.startOfDay
  
  def details :Map[String, Any] = 
    Map("CashInstrumentType" -> cashInstrumentType.name, "Amount" -> volume, "Settlement Date" -> settlementDate) ++
      index.map("Market" -> _).toMap ++
      averagingPeriod.map("Period" -> _).toMap

  def instrumentType = CashInstrument

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(settlementDate)

  def periodKey = Some(DateRangePeriod(settlementDate))

  def price(env : Environment) = Quantity.NULL
}

/**
 * isNegative means that this cash instrument should always be negative, for example a broker cost
 * is always negative (from our point of view)
 */
case class CashInstrumentType(name: String, isNegative: Boolean = false)

object CashInstrumentType {
  val BrokerPayment = CashInstrumentType("Broker Payment", true)
  val ClearingHousePayment = CashInstrumentType("Clearing House Payment", true)
  val General = CashInstrumentType("General")
  val Premium = CashInstrumentType("Premium")
  val Commission = CashInstrumentType("Commission")

  val costTypes = Set(BrokerPayment, ClearingHousePayment, Premium, Commission)

  def isCost(cashInstrumentType: CashInstrumentType) = costTypes.contains(cashInstrumentType)
}

object CashInstrument extends InstrumentType[CashInstrument] {
  val name = "Cash Instrument"
}
