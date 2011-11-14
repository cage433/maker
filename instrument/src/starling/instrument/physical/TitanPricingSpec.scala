package starling.instrument.physical

import starling.curves.Environment
import starling.market.{FuturesMarket, CommodityMarket, FuturesExchangeFactory, IndexWithDailyPrices}
import starling.daterange._
import starling.quantity.{Ratio, UOM, Quantity}
import starling.quantity.UOM._

trait TitanPricingSpec {

  def priceExcludingVAT(env : Environment) : Quantity
  def priceIncludingVAT(env : Environment) : Option[Quantity] 
  def settlementDay(marketDay: DayAndTime): Day

  def premiumCCY: Option[UOM] = {
    premium.uom.numeratorUOM match {
      case UOM.NULL => None
      case ccy => Some(ccy)
    }
  }

  def valuationCCY: UOM
  assert(valuationCCY != null, "Valuation currency is null")

  protected def inValuationCurrency(env : Environment, p : Quantity) = {
    if (p == Quantity.NULL)
      p
    else {
      val namedEnv = env.withNaming()
      val baseCurrency = p.numeratorUOM.inBaseCurrency
      val priceInBaseCurrency = p inUOM (baseCurrency / p.denominatorUOM)
      val fxRate = namedEnv.forwardFXRate(valuationCCY, priceInBaseCurrency.numeratorUOM, settlementDay(namedEnv.marketDay)) 

      priceInBaseCurrency * fxRate
    }
  }

  def addPremiumConvertingIfNecessary(env: Environment, price: Quantity, premium: Quantity): Quantity = {
    val priceInValuationCurrency = inValuationCurrency(env, price)

    if (premium == Quantity.NULL) // no premium case
      priceInValuationCurrency 
    else 
      priceInValuationCurrency + inValuationCurrency(env, premium.named("Premium"))
  }

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity): Quantity

  def isComplete(marketDay: DayAndTime): Boolean

  def pricingType: String

  def daysForPositionReport(marketDay: DayAndTime): List[Day]

  def quotationPeriod: Option[DateRange]

  // Working assumption is that premium is exclusive of VAT
  def premium: Quantity

  def indexOption: Option[IndexWithDailyPrices]

  protected def isLiableToShanghaiVAT = {
    indexOption match {
      case Some(index) => index.market match{
        case fm : FuturesMarket => fm.exchange == FuturesExchangeFactory.SHFE  || fm.exchange == FuturesExchangeFactory.EXBXG
        case _ => false
      }
      case None => false
    }
  }
  protected def subtractVATIfLiable(env : Environment, price : Quantity) = {
    if (isLiableToShanghaiVAT)
      price / (Quantity(100, PERCENT) + env.shanghaiVATRate).named("VAT")
    else
      price
  }

  protected def addVATIfLiable(env : Environment, price : Quantity) = {
    if (isLiableToShanghaiVAT)
      price * (Quantity(100, PERCENT) + env.shanghaiVATRate).named("VAT")
    else
      price
  }
  def expiryDay: Day
  def futuresMarket : FuturesMarket 
}

object TitanPricingSpec {
  def calcSettlementDay(index: IndexWithDailyPrices, day: Day) = {
    val lme = FuturesExchangeFactory.LME
    val market: CommodityMarket = index.market
    market.asInstanceOf[FuturesMarket].exchange match {
      case `lme` => day.addBusinessDays(market.businessCalendar, 2)
      case _ => day
    }
  }

  /**
   * Used for unknown pricing spec and also benchmarks
   * Nothing in common really and its use for the latter is probably wrong
   */
  def representativeDay(index :IndexWithDailyPrices, month : Month, marketDay : DayAndTime) : Day = {
    val lme = FuturesExchangeFactory.LME
    index.market.asInstanceOf[FuturesMarket].exchange match {
      case `lme` => {
        val thirdWednesday = month.firstDay.dayOnOrAfter(DayOfWeek.wednesday) + 14
        if (marketDay >= thirdWednesday.endOfDay)
          month.lastDay.thisOrPreviousBusinessDay(index.businessCalendar)
        else
          thirdWednesday
      }
      case _ => month.lastDay.thisOrPreviousBusinessDay(index.market.businessCalendar)
    }
  }
}

case class AveragePricingSpec(index: IndexWithDailyPrices, period: DateRange,
                              premium: Quantity, valuationCCY : UOM) extends TitanPricingSpec {
  val observationDays = index.observationDays(period)
  // Just a guess
  def settlementDay(marketDay: DayAndTime) = TitanPricingSpec.calcSettlementDay(index, period.lastDay)

  def fixedQuantity(marketDay: DayAndTime,
                    totalQuantity: Quantity): Quantity = totalQuantity * observedDays(marketDay).size / observationDays.size

  def pricingType: String = "Month Average"

  protected def observedDays(marketDay: DayAndTime) = observationDays.filter(_.endOfDay <= marketDay)

  def isComplete(marketDay: DayAndTime) = observedDays(marketDay).size == observationDays.size

  def indexOption = Some(index)

  def quotationPeriod = Some(period)

  def daysForPositionReport(marketDay: DayAndTime) = observationDays.filter(_.endOfDay > marketDay)


  def expiryDay = TitanPricingSpec.calcSettlementDay(index, period.lastDay)
  def priceExcludingVAT(env : Environment) = {
    val priceExclVAT = subtractVATIfLiable(env, env.averagePrice(index, period))
    addPremiumConvertingIfNecessary(env, priceExclVAT, premium)
  }
  def priceIncludingVAT(env : Environment) = {
    if (isLiableToShanghaiVAT){
      val premiumInclVAT = addVATIfLiable(env, premium)
      Some(addPremiumConvertingIfNecessary(env, env.averagePrice(index, period), premiumInclVAT))
    } else {
      None
    }
  }
  def futuresMarket : FuturesMarket = index.market.asInstanceOf[FuturesMarket]
}

case class InvalidTitanPricingSpecException(msg: String) extends Exception(msg)

case class FixedPricingSpec(futuresMarket : FuturesMarket, settDay: Day, pricesByFraction: List[(Double, Quantity)],
                            premium: Quantity, valuationCCY : UOM) extends TitanPricingSpec {

  def settlementDay(marketDay: DayAndTime) = settDay

  private def priceExclPremium(env : Environment) : Quantity = {
    val totalFraction = pricesByFraction.map(_._1).sum
    if (totalFraction == 0) {
      throw new InvalidTitanPricingSpecException("Fixed Pricing Spec with no fixed prices")
    } else {
      Quantity.sum(pricesByFraction.zipWithIndex.map {
        case ((qty, prc), i) => prc.named("F_" + i) * qty
      }) / totalFraction
    }
  }

  def priceExcludingVAT(env : Environment) = {
    addPremiumConvertingIfNecessary(env, priceExclPremium(env), premium)
  }
  def priceIncludingVAT(env : Environment) = None

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = totalQuantity

  def isComplete(marketDay: DayAndTime) = true

  def pricingType: String = "Fixed"

  def quotationPeriodStart: Option[Day] = None

  def quotationPeriodEnd: Option[Day] = None

  def indexName: String = "No Index"

  def daysForPositionReport(marketDay: DayAndTime) = List(marketDay.day)

  def quotationPeriod = None

  def indexOption = None

  def expiryDay = settDay
}


case class UnknownPricingFixation(fraction: Double, price: Quantity)


case class UnknownPricingSpecification(
                                        index: IndexWithDailyPrices,
                                        month: Month,
                                        fixations: List[UnknownPricingFixation],
                                        declarationDay: Day,
                                        premium: Quantity,
                                        valuationCCY : UOM
                                        )
  extends TitanPricingSpec {

  def settlementDay(marketDay: DayAndTime) = TitanPricingSpec.calcSettlementDay(index, unfixedPriceDay(marketDay))


  private def unfixedPriceDay(marketDay : DayAndTime) = TitanPricingSpec.representativeDay(index, month, marketDay)

  private def priceExclPremium(env : Environment) = {
    val totalFixed = fixations.map(_.fraction).sum
    val unfixedFraction = 1.0 - totalFixed
    val fixedPayment = Quantity.sum(fixations.zipWithIndex.map {
      case (f, i) => f.price.named("Fix_" + i) * f.fraction
    }).named("Fixed")
    val unfixedPayment = (index.fixingOrForwardPrice(env, unfixedPriceDay(env.marketDay)) * unfixedFraction).named("Unfixed")
    inValuationCurrency(env, fixedPayment) + inValuationCurrency(env, unfixedPayment)
  }

  def priceExcludingVAT(env: Environment) = {
    val price = priceExclPremium(env)
    addPremiumConvertingIfNecessary(env, subtractVATIfLiable(env, price), premium)
  }

  def priceIncludingVAT(env: Environment) = {
    if (isLiableToShanghaiVAT){
      val price = priceExclPremium(env)
      Some(addPremiumConvertingIfNecessary(env, price, addVATIfLiable(env, premium)))
    } else {
      None
    }
  }
  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = totalQuantity * fixations.map(_.fraction).sum

  def isComplete(marketDay: DayAndTime) = declarationDay.endOfDay >= marketDay

  def pricingType: String = "Unknown"

  def quotationPeriodStart: Option[Day] = Some(month.firstDay)

  def quotationPeriodEnd: Option[Day] = Some(month.lastDay)

  def indexName: String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = index.observationDays(month).filter(_.endOfDay > marketDay)

  def quotationPeriod = Some(month)

  def indexOption = Some(index)

  def expiryDay = TitanPricingSpec.calcSettlementDay(index, index.observationDays(month).last)

  def futuresMarket : FuturesMarket = index.market.asInstanceOf[FuturesMarket]
}





