package starling.instrument

import starling.curves.Environment
import starling.richdb.RichInstrumentResultSetRow
import starling.daterange.{Month, DateRange, Day, DayAndTime}
import starling.quantity.RichQuantity._
import starling.instrument.CashInstrumentType._
import starling.quantity._

/**
 * To add costs, create this with any name, a pay rule that fits and an appropriate
 * commission. New pay rules and commissions can be added without any side-effects.
 */
abstract class Costs(cashInstrumentType: CashInstrumentType, counterParty: String, payRule: PayRule, commission: CostAmountCalc) extends AsUtpPortfolio {
  def costs:List[CashInstrument] = payRule.costs(cashInstrumentType, commission)

  def mtm(env: Environment, ccy: UOM) = Quantity.sum(Quantity(0, ccy) :: costs.map(cost => {
    val mtm = cost.mtm(env)
    mtm * env.spotFXRate(ccy, mtm.uom)
  }))

  def explanation(env: Environment): NamedQuantity = {
    new SimpleNamedQuantity("Costs", mtm(env, UOM.USD))
  }

  def asUtpPortfolio(tradeDay:Day) = {
    val utps: Map[UTP, Double] = costs.map {
      case b: CashInstrument => {
        (b.copy(volume = new Quantity(1.0, b.volume.uom)) -> b.volume.value)
      }
    }.toMap
    UTP_Portfolio(utps)
  }

  override def toString = (cashInstrumentType, counterParty, payRule, commission).toString

  def costType = cashInstrumentType.name
  def info():List[(String,String)]
}

case class ClearingHouseCosts(clearingHouse: String, payRule: PayRule, commission: CostAmountCalc) extends Costs(ClearingHousePayment, clearingHouse, payRule, commission) {
  def info() = throw new Exception("We can't deal with clearing house costs yet")
}
case class BrokerCosts(broker: String, payRule: PayRule, commission: CostAmountCalc) extends Costs(BrokerPayment, broker, payRule, commission) {
  def info() = throw new Exception("We can't deal with broker costs yet")
}
case class PremiumCosts(settlementDay: Day, counterParty: String, volume: Quantity, premium: Quantity)
        extends Costs(Premium, counterParty, new SinglePayment(settlementDay, volume), new PremiumLumpSumAmountMultiple(premium)) {
  override def toString = "Premium(" + premium + ", " + counterParty + ", " + volume + ", " + settlementDay + ")"
  def info() = List(
    ("Settlement Day", settlementDay.toString),
    ("Counter Party", counterParty)
  )
}
case class CommissionCosts(name: String, code: String, settlementDay: Day, counterParty: String, commission: Quantity)
        extends Costs(Commission, counterParty, new SinglePayment(settlementDay), new CommissionLumpSum(commission)) {
  override def toString = code + "(" + name + ", " + commission + ", " + settlementDay + ")"
  def info() = List(
    ("Name", name),
    ("Settlement Day", settlementDay.toString),
    ("Counter Party", counterParty)
  )
}

case class OrdinaryCost(name: String, code: String, settlementDay: Day, counterParty: String, quantity: Quantity)
  extends Costs(Ordinary, counterParty, new SinglePayment(settlementDay), new CommissionLumpSum(quantity)) {
  override def toString = code + "(" + name + ", " + quantity + ", " + settlementDay + ")"

  def info() = List(
    ("Name", name),
    ("Settlement Day", settlementDay.toString),
    ("Counter Party", counterParty)
  )
}


/**
 * Pay rules define how many costs, and how much, should be paid given a commission.
 */
trait PayRule {
  def costs(cashInstrumentType: CashInstrumentType, commission: CostAmountCalc): List[CashInstrument] = {
    cashInstruments(cashInstrumentType, commission).map {
      ci => {
        // some costs, like broker costs, are always negative.
        if (cashInstrumentType.isNegative && ci.volume.isPositve) {
          ci.copy(volume = ci.volume.negate)
        } else {
          ci
        }
      }
    }
  }

  protected def cashInstruments(cashInstrumentType: CashInstrumentType, commission: CostAmountCalc): List[CashInstrument]
}

case class SingleFreightPayment(settlementDate: Day, freightVolume: FreightVolume) extends PayRule {
  def cashInstruments(cashInstrumentType: CashInstrumentType, commission: CostAmountCalc) = {
    val amount = commission.amount(freightVolume.totalAmount)
    List(CashInstrument(cashInstrumentType, amount, settlementDate))
  }
}

case class SinglePayment(settlementDate: Day, volume: Quantity = Quantity.NULL) extends PayRule {
  def cashInstruments(cashInstrumentType: CashInstrumentType, commission: CostAmountCalc) = {
    val amount = commission.amount(volume)
    List(CashInstrument(cashInstrumentType, amount, settlementDate))
  }
}

case class AtMonthEndFreight(period: DateRange, freightVolume: FreightVolume) extends PayRule {
  def cashInstruments(cashInstrumentType: CashInstrumentType, commission: CostAmountCalc) = freightVolume.months.map {
    month => {
      val amount = commission.amount(freightVolume.amount(month))
      CashInstrument(cashInstrumentType, amount, month.lastDay)
    }
  }
}

/**
 * A commission can be a percentage or lump sum.
 */
trait CostAmountCalc {
  def amount(volume: Quantity): Quantity
}

case class CommissionPercent(percent: Percentage, strike: Quantity, ccy: UOM) extends CostAmountCalc {
  def amount(volume: Quantity) = {
    strike * volume.abs * percent
  }
}

case class CommissionLumpSum(lump: Quantity) extends CostAmountCalc {
  def amount(volume: Quantity) = lump
}

case class CommissionLumpSumAmountMultiple(lump: Quantity) extends CostAmountCalc {
  def amount(volume: Quantity) = lump * volume.abs
}

case class PremiumLumpSumAmountMultiple(premiumPrice: Quantity) extends CostAmountCalc {
  // 'volume' is the trade's volume, premiumPrice is always positive, hence the negation 
  def amount(volume: Quantity) = premiumPrice * volume * -1
}
