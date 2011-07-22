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
	extends UTP with Tradeable
{
  def this(amount : Quantity, settlementDate : Day) = this(CashInstrumentType.General, amount, settlementDate)

  def *(x : Double) = copy(volume = volume * x)

  def assets(env : Environment) = Assets(Asset.knownCash(settlementDate, volume, env))
  
	def valuationCCY : UOM = volume.uom
	def isLive(dayAndTime : DayAndTime) : Boolean = dayAndTime <= settlementDate.startOfDay
  
  def detailsForUTPNOTUSED :Map[String, Any] = persistedTradeableDetails


  def instrumentType = CashInstrument

  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(settlementDate)

  def periodKey = Some(DateRangePeriod(settlementDate))

  def price(env : Environment) = Quantity.NULL

  def asUtpPortfolio(tradeDay: Day) = {
    val unit = copy(volume = Quantity(1, valuationCCY))
    UTP_Portfolio(Map(unit -> volume.value))
  }

  def persistedTradeableDetails = Map("CashInstrumentType" -> cashInstrumentType.name, "Quantity" -> volume, "Delivery Day" -> settlementDate) ++
      index.map("Market" -> _).toMap ++
      averagingPeriod.map("Period" -> _).toMap

  def tradeableType = CashInstrument

  override def expiryDay() = Some(settlementDate)
}

object CashInstrument extends InstrumentType[CashInstrument] with TradeableType[CashInstrument]  {

  def sample = CashInstrument(CashInstrumentType.Ordinary, Quantity(1, USD), Day(2011, 1, 1))

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val ciType = CashInstrumentType.fromString(row.getString("CashInstrumentType")).get
    val quantity = row.getQuantity("quantity")
    val settlementDay = row.getDay("deliveryDay")

    val market = try {
      Some(Right(row.getMarket("market")))
    } catch {
      case _ => try {
        Some(Left(row.getIndexFromName("market")))
      } catch {
        case _ => None
      }
    }
    val period = if(row.isNull("period")) None else Some(row.getPeriod("period"))

    CashInstrument(ciType, quantity, settlementDay, market, period)
  }

  val name = "Cash Instrument"
}

/**
 * isNegative means that this cash instrument should always be negative, for example a broker cost
 * is always negative (from our point of view)
 */
case class CashInstrumentType(name: String, isNegative: Boolean = false)

object CashInstrumentType {
  val BrokerPayment = CashInstrumentType("Broker Payment", true)
  val ClearingHousePayment = CashInstrumentType("Clearing House Payment", true)
  val Premium = CashInstrumentType("Premium")
  val Commission = CashInstrumentType("Commission")
  val Ordinary = CashInstrumentType("Ordinary Cost")

  val costTypes = Set(BrokerPayment, ClearingHousePayment, Premium, Commission, Ordinary)

  val General = CashInstrumentType("General")
  val nonCostTypes = Set(General)

  val allTypes = costTypes ++ nonCostTypes

  def isCost(cashInstrumentType: CashInstrumentType) = costTypes.contains(cashInstrumentType)

  def fromString(s: String): Option[CashInstrumentType] = {
    allTypes.find(_.name.equalsIgnoreCase(s))
  }
}
