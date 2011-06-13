package starling.instrument

/**
 * Currently very similar to RefinedAssignment - they should diverge once we do more than
 * the risk card
 */
import starling.quantity.UOM.USD
import starling.richdb.RichInstrumentResultSetRow
import starling.quantity.{UOM, Quantity}
import starling.daterange.{DateRange, Month, Day, DayAndTime}
import starling.market._
import starling.utils.StarlingXStream
import java.lang.String
import starling.curves.Environment
import starling.daterange.DateRangePeriod

/**
 * TODO [11 Aug 2010] find out what it is that is fixed for daily markets. e.g. third wednesday. It is it the Thursday futures
 * price observed on Wednesday, or the Wednesday price observed on Tuesday, or even the Wednesday price observed on Wednesday
 * For now we will assume the first
 *
 * Rule is this
 *  if LME
 *    if CONTRA_AVERAGE = 'Y'
 *      Third Wednesday of month of fixation date
 *    else
 *      cash prompt date, i.e. two business days after fixation date
 *  else
 *    normally (but not always) the last calendar day (not business day) of the fixation date month
 *
 * TODO [11 Aug 2010] speak to Chris G about all this
 *
 */

case class RefinedFixationsForSplit(
  fixations : List[RefinedFixation]
) extends Tradeable{
  def isLive(dayAndTime: DayAndTime) = fixations.forall(_.isLive(dayAndTime))

  lazy val tradeableDetails = {
    val serialised: String = StarlingXStream.write(fixations)
    Map[String, Any]("Fixations" -> serialised)
  }

  def asUtpPortfolio(tradeDay:Day) = {
    val map = scala.collection.mutable.Map[UTP, Double]()
    fixations.foreach{
      case RefinedFixation(market, fixationDate, isAverage, volume) =>
        val utp = RefinedFixation(market, fixationDate, isAverage, Quantity(1.0, volume.uom))
        map(utp) = volume.value + map.getOrElse(utp, 0.0)
    }
    UTP_Portfolio(Map() ++ map)
  }

  def tradeableType = RefinedFixationsForSplit

  override def expiryDay():Option[Day] = fixations match {
    case Nil => None
    case _ => Some(fixations.map(_.fixationDate).reduceLeft(_ max _))
  }

}

object RefinedFixationsForSplit extends TradeableType[RefinedFixationsForSplit]{
  def sample = RefinedFixationsForSplit(List(RefinedFixation(Market.LME_LEAD, Day(2010, 1, 1), "Y", Quantity(1.0, UOM.MT))))

  def createTradeable(row: RichInstrumentResultSetRow) = RefinedFixationsForSplit(
    StarlingXStream.read(row.getString("Fixations")).asInstanceOf[List[RefinedFixation]]
  )

  val name = "Refined Unpriced"
}



case class RefinedFixation(
  market : FuturesMarket,
  fixationDate : Day,
  isAverage : String,
  volume : Quantity
)
  extends UTP
{


  /*
    I don't think is meaningful, but it is what happens currently in the risk card - except for the COMEX bit, which
    is almost always the last day. No pattern to it.
   */

  val pricePeriod : DateRange = {
    import FuturesExchangeFactory.{LME, COMEX}
    val month = fixationDate.containingMonth
    (market.exchange, isAverage) match {
      case (LME, "Y") => month.thirdWednesday max (fixationDate.addBusinessDays(market.businessCalendar, 2))
      case (LME, "N") => fixationDate.addBusinessDays(market.businessCalendar, 2)
      case (COMEX, _) => month
    }
  }
  private val index = FuturesFrontPeriodIndex(market)
  // Currently Starling only has indices for front periods. Not sure that pricePeriods are in fact front periods,
  // until we understand properly how refined contracts are priced, we'll use the price for the riskPeriod observed
  // on its last trading day.

  private val fixingDayForPeriod = market.lastTradingDay(pricePeriod)

  // TODO [11 Aug 2010] find out the correct settlement day
  private val settlementDay = fixingDayForPeriod

  def isLive(dayAndTime: DayAndTime) = fixationDate.endOfDay > dayAndTime

  def valuationCCY = market.currency

  def price(env: Environment): Quantity = {
    if (env.marketDay < fixingDayForPeriod.endOfDay) {
      env.forwardPrice(market, pricePeriod)
    } else {
      env.fixingOrForwardPrice(index, fixingDayForPeriod)
    }
  }

  def assets(env: Environment) = {
    val F = price(env)
    market.convert(volume, market.uom) match {
      case Some(volumeInMarketUnits) => {
        if (env.marketDay < fixingDayForPeriod.endOfDay) {
          Assets(Asset.estimatedCash(settlementDay, F * volume, env))
        } else {
          Assets(Asset.knownCash(settlementDay, F * volume, env))
        }
      }
      case None => {
        throw new Exception("Volume UOM = " + volume.uom + ", market uom = " + market.uom)
      }
    }
  }

  def details : Map[String, Any] = Map("Fixation Date" -> fixationDate, "Market" -> market, "Is Average Fixation" -> isAverage)


  def instrumentType = RefinedFixation


  def daysForPositionReport(marketDay : DayAndTime) : Seq[Day] = List(fixationDate)

  def * (scale : Double) = copy(volume = volume * scale)

  def periodKey = Some(DateRangePeriod(fixationDate))

}


object RefinedFixation extends InstrumentType[RefinedFixation]  {
   val name = "Refined Fixation"
}

