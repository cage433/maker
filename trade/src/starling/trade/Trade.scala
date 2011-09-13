package starling.trade

import starling.curves.{Environment}
import java.lang.String
import starling.pivot.Field._
import starling.instrument._
import starling.daterange.{DateRangePeriod, Day, DayAndTime}
import starling.quantity.{SimpleNamedQuantity, UOM, Quantity}

/**Fairly obvious trade class. TradeAttributes are system dependent and can be things
 * like 'Portfolio', 'Trader', 'Strategy'
 */
case class Trade(
        tradeID: TradeID,
        tradeDay: Day,
        counterParty: String,
        @transient attributes: TradeAttributes,
        tradeable: Tradeable,
        costs: List[Costs] = Nil
        )
 extends Instrument {
  def copyWithInstrument(instrument: Tradeable) = copy(tradeable = instrument)

  lazy val expiryDay = tradeable.expiryDay
  def isErrorTrade = tradeable.isInstanceOf[ErrorInstrument]
  def isDeletedTrade = tradeable.isInstanceOf[DeletedInstrument]

  protected def explanation(env: Environment) = {
    val tr = tradeable.explanation(env)
    val pnl = costs match {
      case Nil => tr
      case h::Nil => tr + h.explanation(env)
      case _ => tr + new SimpleNamedQuantity("All Costs", Quantity.sumNamed(costs.map(_.explanation(env))))
    }
    pnl.named("P&L")
  }

  def assets(env: Environment) = asUtpPortfolio.assets(env)

  /**Determines if a trade is live. A trade is not live on a given day if either
   * 		a) The trade day is AFTER the given day (time of day is not used in this comparison)
   * 		b) the underlying instrument has expired on the given day and time of day
   */
  def isLive(dayAndTime: DayAndTime) = {
    tradeDay <= dayAndTime.day && tradeable.isLive(dayAndTime)
  }

  override def valuationCCY = tradeable.asUtpPortfolio(tradeDay).valuationCCY

  override def atomicMarketDataKeys(marketDay: DayAndTime, ccy : UOM = UOM.USD) = tradeable.asUtpPortfolio(tradeDay).atomicMarketDataKeys(marketDay, ccy)

  def asUtpPortfolio = {
    val utpPortfolio = (new UTP_Portfolio() /: (tradeable:: costs).map(_.asUtpPortfolio(tradeDay)))(_ ++ _)
    val fixUpCashInstruments = utpPortfolio.portfolio.map {
      case (utp,volume) => {
        val fixedUTP = utp match {
          case ci:CashInstrument => {
            tradeable.fixUpCashInstruments(ci)
          }
          case other => other
        }
        fixedUTP -> volume
      }
    }
    UTP_Portfolio(fixUpCashInstruments)
  }

  def deltaStepType() = throw new UnsupportedOperationException()

  def premium: Option[Quantity] = Trade.extractPremium(costs)
}

object Trade {
  val fields = List((tradeID_str, classOf[String]),
    (tradeDay_str, classOf[Day]), (counterparty_str, classOf[String]), (instrument_str, classOf[String]), (costs_str, classOf[String]))
  val fieldNames = fields.map(_._1)

  def extractPremium(costs:List[Costs]) = {
    val premiums = costs.flatMap {
      case PremiumCosts(_, _, _, premium) => Some(premium)
      case _ => None
    }
    premiums match {
      case Nil => None
      case p :: Nil => Some(p)
      case _ => throw new Exception("Can't handle multiple premiums")
    }
  }
}


