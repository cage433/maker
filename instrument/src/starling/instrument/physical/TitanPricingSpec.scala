package starling.instrument.physical

import starling.curves.Environment
import starling.market.{FuturesMarket, CommodityMarket, FuturesExchangeFactory, IndexWithDailyPrices}
import starling.daterange._
import starling.market.FuturesExchange
import starling.quantity.{UOM, Quantity}

trait TitanPricingSpec {

//  def priceExcludingPremium(env : Environment) : Quantity

  def premiumExcludingVAT(env : Environment) : Quantity = {
    val premiumInPremiumCurrency = if (isLiableToShanghaiVAT) (premium(env).named("Premium") / toDisplayVat(env.shanghaiVATRate)).named("Premium Excl VAT") else premium(env).named("Premium")
    inValuationCurrency(env, premiumInPremiumCurrency)
  }

  def premiumIncludingVAT(env : Environment) : Option[Quantity] = {
    if (isLiableToShanghaiVAT)
      Some(premiumExcludingVAT(env) * toDisplayVat(env.shanghaiVATRate))
    else
      None
  }

  // ensure VAT is displayed in explanation in the form 120% rather than 20%, 0.2 + 1.0 or similar
  private def toDisplayVat(vat : Quantity) = (vat + 1.0).unnamed.named("VAT")

  def priceExcludingVATExcludingPremium(env: Environment): Quantity

  def priceExcludingVATIncludingPremium(env : Environment) : Quantity = priceExcludingVATExcludingPremium(env) + premiumExcludingVAT(env)

  def priceIncludingVATExcludingPremium(env : Environment) = {
    if (isLiableToShanghaiVAT){
      Some(priceExcludingVATExcludingPremium(env) * toDisplayVat(env.shanghaiVATRate))
    } else {
      None
    }
  }

  def priceIncludingVATIncludingPremium(env : Environment) = {
    priceIncludingVATExcludingPremium(env).map{prc => prc + inValuationCurrency(env, premium(env))}
  }
  def valueExcludingVATExcludingPremium(env : Environment, quantity : Quantity) = {
    priceExcludingVATExcludingPremium(env) * quantity * env.discount(valuationCCY, settlementDay(env.marketDay))
  }
  def valueExcludingVATIncludingPremium(env : Environment, quantity : Quantity) = {
    priceExcludingVATIncludingPremium(env) * quantity * env.discount(valuationCCY, settlementDay(env.marketDay))
  }
  def valueIncludingVATExcludingPremium(env : Environment, quantity : Quantity) = {
    priceIncludingVATExcludingPremium(env).map(_ * quantity * env.discount(valuationCCY, settlementDay(env.marketDay)))
  }
  def valueIncludingVATIncludingPremium(env : Environment, quantity : Quantity) = {
    priceIncludingVATIncludingPremium(env).map(_ * quantity * env.discount(valuationCCY, settlementDay(env.marketDay)))
  }

  def settlementDay(marketDay: DayAndTime): Day

//  def premiumCCY: Option[UOM] = {
//    premium.uom.numeratorUOM match {
//      case UOM.NULL => None
//      case ccy => Some(ccy)
//    }
//  }

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
      priceInValuationCurrency + inValuationCurrency(env, premium)
  }

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity): Quantity

  def isComplete(marketDay: DayAndTime): Boolean

  def pricingType: String

  def daysForPositionReport(marketDay: DayAndTime): List[Day]

  def quotationPeriod: Option[DateRange]

  // Working assumption is that premium is inclusive of VAT when the relevant exchange includes it
  def premium(env : Environment) : Quantity

  def indexOption: Option[IndexWithDailyPrices]

  protected def isExchangeLiableToVAT(exchange : FuturesExchange) = exchange == FuturesExchangeFactory.SHFE  || exchange == FuturesExchangeFactory.EXBXG

  protected def isLiableToShanghaiVAT = {
    indexOption match {
      case Some(index) => index.market match{
        case fm : FuturesMarket => isExchangeLiableToVAT(fm.exchange)
        case _ => false
      }
      case None => false
    }
  }
  protected def subtractVATIfLiable(env : Environment, price : Quantity) = {
    if (isLiableToShanghaiVAT)
      price / toDisplayVat(env.shanghaiVATRate)
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

  def premium(env : Environment) = premium

  def expiryDay = TitanPricingSpec.calcSettlementDay(index, period.lastDay)
  def priceExcludingVATExcludingPremium(env : Environment) = {
    inValuationCurrency(env, subtractVATIfLiable(env, env.averagePrice(index, period)))
//    val premiumExclVAT = subtractVATIfLiable(env, premium.named("Premium"))
//    addPremiumConvertingIfNecessary(env, priceExclVAT, premiumExclVAT)
  }
  def futuresMarket : FuturesMarket = index.market.asInstanceOf[FuturesMarket]
}

case class OptionalPricingSpec(choices: List[TitanPricingSpec], declarationDay: Day,
                               chosenSpec: Option[TitanPricingSpec]) extends TitanPricingSpec {

  lazy val specToUse = chosenSpec.getOrElse(choices.head)

  def futuresMarket = specToUse.futuresMarket

  def settlementDay(marketDay: DayAndTime) = specToUse.settlementDay(marketDay)

  def priceExcludingVATExcludingPremium(env: Environment) = {
    assert(chosenSpec.isDefined || env.marketDay < declarationDay.endOfDay, "Optional pricing spec must be fixed by " + declarationDay)
    inValuationCurrency(env, specToUse.priceExcludingVATExcludingPremium(env))
  }

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = specToUse.fixedQuantity(marketDay, totalQuantity)

  def isComplete(marketDay: DayAndTime) = specToUse.isComplete(marketDay)

  def pricingType: String = chosenSpec match {
    case Some(spec) => spec.pricingType;
    case None => "Optional"
  }

  def premium(env : Environment) = specToUse.premium(env)

  def daysForPositionReport(marketDay: DayAndTime) = specToUse.daysForPositionReport(marketDay)

  def valuationCCY = specToUse.valuationCCY

  def quotationPeriod = specToUse.quotationPeriod

  def indexOption = specToUse.indexOption

  def expiryDay = specToUse.expiryDay
}

case class WeightedPricingSpec(specs: List[(Double, TitanPricingSpec)], valuationCCY : UOM) extends TitanPricingSpec {
  def settlementDay(marketDay: DayAndTime) = specs.flatMap(_._2.settlementDay(marketDay)).sortWith(_ > _).head

  def priceExcludingVATExcludingPremium(env: Environment) = Quantity.sum(specs.map {
    case (weight, spec) => inValuationCurrency(env, spec.priceExcludingVATExcludingPremium(env)) * weight
  })

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = specs.map {
    case (wt, spec) => spec.fixedQuantity(marketDay, totalQuantity) * wt
  }.sum

  def isComplete(marketDay: DayAndTime) = specs.forall {
    _._2.isComplete(marketDay)
  }

  def pricingType: String = "Weighted"

  private def quotationPeriodStart: Option[Day] = specs.map(_._2.quotationPeriod).collect {
    case Some(period) => period.firstDay
  }.sortWith(_ < _).headOption

  private def quotationPeriodEnd: Option[Day] = specs.map(_._2.quotationPeriod).collect {
    case Some(period) => period.lastDay
  }.sortWith(_ < _).lastOption

  override def premium(env : Environment) = specs.map {
    case (wt, spec) => inValuationCurrency(env, spec.premium(env)) * wt
  }.sum

  def daysForPositionReport(marketDay: DayAndTime) = specs.flatMap {
    case (amt, spec) => spec.daysForPositionReport(marketDay)
  }.distinct

  def quotationPeriod = (quotationPeriodStart, quotationPeriodEnd) match {
    case (Some(d1), Some(d2)) => Some(DateRange(d1, d2))
    case _ => None
  }

  def indexOption = None

  def expiryDay = specs.map(_._2.expiryDay).max

  def futuresMarket = {
    val markets = specs.map(_._2.futuresMarket).distinct
    assert(markets.size == 1, "Expected a single associated futures market - got " + markets.mkString(", "))
    markets.head
  }

  override def isLiableToShanghaiVAT = isExchangeLiableToVAT(futuresMarket.exchange)
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
        case ((qty, prc), i) => inValuationCurrency(env, prc.named("Fix " + i)) * qty
      }) / totalFraction
    }
  }

  def priceExcludingVATExcludingPremium(env : Environment) = {
    inValuationCurrency(env, subtractVATIfLiable(env, priceExclPremium(env)))
    //, subtractVATIfLiable(env, premium.named("Premium")))
  }

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

  override def isLiableToShanghaiVAT = isExchangeLiableToVAT(futuresMarket.exchange)

  def premium(env : Environment) = premium
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

  def priceExcludingVATExcludingPremium(env: Environment) = {
    inValuationCurrency(env, subtractVATIfLiable(env, priceExclPremium(env)))
//    val price = priceExclPremium(env)
//    addPremiumConvertingIfNecessary(env, subtractVATIfLiable(env, price), subtractVATIfLiable(env, premium.named("Premium")))
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

  def premium(env : Environment) = premium
}
