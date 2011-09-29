package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.quantity.{UOM, Quantity}
import starling.utils.sql.PersistAsBlob
import starling.quantity.NamedQuantity
import starling.market._
import starling.titan.valuation.{CostsAndIncomeAllocatedSaleAssignmentValuation, CostsAndIncomeAllocatedPurchaseAssignmentValuation, PricingValuationDetails, CostsAndIncomeUnallocatedAssignmentValuation}

trait TitanPricingSpec {

  import TitanPricingSpec._

  def price(env: Environment): Quantity

  def settlementDay(marketDay: DayAndTime): Day

  def premiumCCY: Option[UOM] = {
    premium.uom.numeratorUOM match {
      case UOM.NULL => None
      case ccy => Some(ccy)
    }
  }

  def valuationCCY: UOM

  def addPremiumConvertingIfNecessary(env: Environment, price: Quantity, premium: Quantity): Quantity = {
    if (premium == Quantity.NULL)
      price
    else {
      val premiumCurrency = premium.uom.numeratorUOM
      val priceCurrency = price.uom.numeratorUOM
      price * env.forwardFXRate(premiumCurrency, priceCurrency, settlementDay(env.marketDay)) + premium.named("Premium")
    }
  }

  // This will go once EDM trades have both pricing specs
  def dummyTransferPricingSpec: TitanPricingSpec

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity): Quantity

  def isComplete(marketDay: DayAndTime): Boolean

  def pricingType: String

  def daysForPositionReport(marketDay: DayAndTime): List[Day]

  def quotationPeriod: Option[DateRange]

  def premium: Quantity

  def indexOption: Option[IndexWithDailyPrices]

  def expiryDay: Day
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
}

case class AveragePricingSpec(index: IndexWithDailyPrices, period: DateRange,
                              premium: Quantity) extends TitanPricingSpec {
  val observationDays = index.observationDays(period)
  // Just a guess
  def settlementDay(marketDay: DayAndTime) = TitanPricingSpec.calcSettlementDay(index, period.lastDay)

  def price(env: Environment) = addPremiumConvertingIfNecessary(env, env.averagePrice(index, period), premium)

  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)

  def fixedQuantity(marketDay: DayAndTime,
                    totalQuantity: Quantity): Quantity = totalQuantity * observedDays(marketDay).size / observationDays.size

  def pricingType: String = "Month Average"

  protected def observedDays(marketDay: DayAndTime) = observationDays.filter(_.endOfDay <= marketDay)

  def isComplete(marketDay: DayAndTime) = observedDays(marketDay).size == observationDays.size

  def indexOption = Some(index)

  def quotationPeriod = Some(period)

  def daysForPositionReport(marketDay: DayAndTime) = observationDays.filter(_.endOfDay > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)

  def expiryDay = TitanPricingSpec.calcSettlementDay(index, period.lastDay)
}


case class OptionalPricingSpec(choices: List[TitanPricingSpec], declarationDay: Day,
                               chosenSpec: Option[TitanPricingSpec]) extends TitanPricingSpec {

  private val specToUse = chosenSpec.getOrElse(choices.head)

  def settlementDay(marketDay: DayAndTime) = specToUse.settlementDay(marketDay)

  def price(env: Environment) = {
    assert(chosenSpec.isDefined || env.marketDay < declarationDay.endOfDay, "Optional pricing spec must be fixed by " + declarationDay)
    specToUse.price(env)
  }

  def dummyTransferPricingSpec = OptionalPricingSpec(choices.map(_.dummyTransferPricingSpec), declarationDay, chosenSpec.map(_.dummyTransferPricingSpec))

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = specToUse.fixedQuantity(marketDay, totalQuantity)

  def isComplete(marketDay: DayAndTime) = specToUse.isComplete(marketDay)

  def pricingType: String = chosenSpec match {
    case Some(spec) => spec.pricingType;
    case None => "Optional"
  }

  def premium = specToUse.premium

  def daysForPositionReport(marketDay: DayAndTime) = specToUse.daysForPositionReport(marketDay)

  def valuationCCY = specToUse.valuationCCY

  def quotationPeriod = specToUse.quotationPeriod

  def indexOption = specToUse.indexOption

  def expiryDay = specToUse.expiryDay
}

case class WeightedPricingSpec(specs: List[(Double, TitanPricingSpec)]) extends TitanPricingSpec {
  def settlementDay(marketDay: DayAndTime) = specs.flatMap(_._2.settlementDay(marketDay)).sortWith(_ > _).head

  def price(env: Environment) = Quantity.sum(specs.map {
    case (weight, spec) => spec.price(env) * weight
  })

  def dummyTransferPricingSpec = WeightedPricingSpec(specs.map {
    case (wt, spec) => (wt, spec.dummyTransferPricingSpec)
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

  def premium = specs.map {
    case (wt, spec) => spec.premium * wt
  }.sum

  def daysForPositionReport(marketDay: DayAndTime) = specs.flatMap {
    case (amt, spec) => spec.daysForPositionReport(marketDay)
  }.distinct

  def valuationCCY = {
    val ccys = specs.map(_._2.valuationCCY).distinct
    assert(ccys.size == 1, "No unique valuation currency")
    ccys.head
  }

  def quotationPeriod = (quotationPeriodStart, quotationPeriodEnd) match {
    case (Some(d1), Some(d2)) => Some(DateRange(d1, d2))
    case _ => None
  }

  def indexOption = None

  def expiryDay = specs.map(_._2.expiryDay).max
}

case class InvalidTitanPricingSpecException(msg: String) extends Exception(msg)

case class FixedPricingSpec(settDay: Day, pricesByFraction: List[(Double, Quantity)],
                            premium: Quantity) extends TitanPricingSpec {

  def settlementDay(marketDay: DayAndTime) = settDay

  def price(env: Environment) = {
    val totalFraction = pricesByFraction.map(_._1).sum
    if (totalFraction == 0) {
      throw new InvalidTitanPricingSpecException("Fixed Pricing Spec with no fixed prices")
    } else {
      val fixedPriceComponent = Quantity.sum(pricesByFraction.zipWithIndex.map {
        case ((qty, prc), i) => prc.named("F_" + i) * qty
      }) / totalFraction
      addPremiumConvertingIfNecessary(env, fixedPriceComponent, premium)
    }
  }

  def dummyTransferPricingSpec = copy()

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = totalQuantity

  def isComplete(marketDay: DayAndTime) = true

  def pricingType: String = "Fixed"

  def quotationPeriodStart: Option[Day] = None

  def quotationPeriodEnd: Option[Day] = None

  def indexName: String = "No Index"

  def daysForPositionReport(marketDay: DayAndTime) = List(marketDay.day)

  def valuationCCY = {
    val ccys = pricesByFraction.map(_._2.numeratorUOM).distinct
    assert(ccys.size == 1, "No unique valuation currency")
    ccys.head
  }

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
                                        premium: Quantity
                                        )
  extends TitanPricingSpec {

  def settlementDay(marketDay: DayAndTime) = TitanPricingSpec.calcSettlementDay(index, unfixedPriceDay(marketDay))


  private def unfixedPriceDay(marketDay : DayAndTime) = {
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
    }}

  def price(env: Environment) = {
    val totalFixed = fixations.map(_.fraction).sum
    val unfixedFraction = 1.0 - totalFixed
    val fixedPayment = Quantity.sum(fixations.zipWithIndex.map {
      case (f, i) => f.price.named("Fix_" + i) * f.fraction
    }).named("Fixed")
    val unfixedPayment = (index.fixingOrForwardPrice(env, unfixedPriceDay(env.marketDay)) * unfixedFraction).named("Unfixed")
    addPremiumConvertingIfNecessary(env, fixedPayment + unfixedPayment, premium)
  }

  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)

  def fixedQuantity(marketDay: DayAndTime, totalQuantity: Quantity) = totalQuantity * fixations.map(_.fraction).sum

  def isComplete(marketDay: DayAndTime) = declarationDay.endOfDay >= marketDay

  def pricingType: String = "Unknown"

  def quotationPeriodStart: Option[Day] = Some(month.firstDay)

  def quotationPeriodEnd: Option[Day] = Some(month.lastDay)

  def indexName: String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = index.observationDays(month).filter(_.endOfDay > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)

  def quotationPeriod = Some(month)

  def indexOption = Some(index)

  def expiryDay = TitanPricingSpec.calcSettlementDay(index, index.observationDays(month).last)
}


trait PhysicalMetalAssignmentOrUnassignedSalesQuota extends UTP with Tradeable{
  def commodityName: String
  def quantity: Quantity
  def contractDeliveryDay: Day
  def contractPricingSpec: TitanPricingSpec
  def benchmarkDeliveryDay : Option[Day]
  def benchmarkPricingSpec: Option[TitanPricingSpec]
  def isPurchase: Boolean

  import PhysicalMetalAssignmentOrUnassignedSalesQuota._

  override def shownTradeableDetails = Map(
      commodityLabel -> commodityName,
      contractDeliveryDayLabel -> contractDeliveryDay,
      contractPricingSpecNameLabel -> contractPricingSpec.pricingType,
      quantityLabel -> quantity,
      premiumLabel -> contractPricingSpec.premium
    ) ++ contractPricingSpec.indexOption.map(indexLabel -> _) ++
      contractPricingSpec.quotationPeriod.map(quotationPeriodLabel -> _) ++
      contractPricingSpec.indexOption.flatMap(_.market.exchangeOption).map(exchangeLabel -> _.name)

  def isLive(dayAndTime: DayAndTime) = !contractPricingSpec.isComplete(dayAndTime) && (benchmarkPricingSpec match {
    case Some(spec) => !spec.isComplete(dayAndTime)
    case None => true
  })

  def valuationCCY = contractPricingSpec.valuationCCY

  def persistedTradeableDetails: Map[String, Any] = Map(
    commodityLabel -> commodityName, contractDeliveryDayLabel -> contractDeliveryDay, quantityLabel -> quantity,
    contractPricingSpecLabel -> PersistAsBlob(contractPricingSpec), isPurchaseLabel -> isPurchase
  ) ++ benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _) ++
    benchmarkPricingSpec.map{spec => benchmarkPricingSpecLabel -> PersistAsBlob(spec)}

  def price(env: Environment) = contractPricingSpec.price(env)

  def periodKey = contractPricingSpec.quotationPeriod.map(DateRangePeriod)

  def volume = quantity
  def isAllocated = ! benchmarkPricingSpec.isDefined
  def detailsForUTPNOTUSED: Map[String, Any] = shownTradeableDetails
  def daysForPositionReport(marketDay: DayAndTime) = contractPricingSpec.daysForPositionReport(marketDay)
  override def expiryDay() = Some(contractPricingSpec.expiryDay)

  private def discount(env: Environment, spec : TitanPricingSpec) = env.discount(valuationCCY, spec.settlementDay(env.marketDay)).named("Discount")

  private def pricingSpecPaymentExplained(env : Environment, spec : TitanPricingSpec) : NamedQuantity = {
    val isContractSpec = spec == contractPricingSpec
    val priceName = if (isContractSpec) "Contract Price" else "Benchmark Price"
    val price = spec.price(env).named(priceName)
    val quantityInMarketUOM = quantity.inUOM(price.denominatorUOM).named("Volume")
    val d = discount(env, contractPricingSpec)

    price * quantityInMarketUOM * d
  }

  def explanation(env: Environment): NamedQuantity = {
    var exp = pricingSpecPaymentExplained(env, contractPricingSpec)
    if (benchmarkPricingSpec.isDefined)
      exp = exp - pricingSpecPaymentExplained(env, benchmarkPricingSpec.get)
    if (isPurchase)
      exp = exp * -1
    exp
  }

  def assets(env: Environment) = {
    def asset(spec : TitanPricingSpec) = Asset(
      known = true,
      assetType = commodityName,
      settlementDay = contractDeliveryDay,
      amount = quantity,
      mtm = pricingSpecPaymentExplained(env, spec)
    )
    Assets(
      List(asset(contractPricingSpec) * (if (isPurchase) -1.0 else 1.0))
        ::: benchmarkPricingSpec.map {
          spec => List(asset(spec) * (if (isPurchase) 1.0 else -1.0))
        }.getOrElse(Nil)
      )
  }


}


case class UnallocatedSalesQuota(   commodityName: String,
                                    quantity: Quantity,
                                    contractDeliveryDay: Day,
                                    contractPricingSpec: TitanPricingSpec,
                                    benchmarkDeliveryDay : Option[Day],
                                    benchmarkPricingSpec: Option[TitanPricingSpec]
)
 extends PhysicalMetalAssignmentOrUnassignedSalesQuota with UTP with Tradeable {

  assert(benchmarkPricingSpec.isDefined, "Unassigned sale requires a benchmark pricing spec")
  assert(benchmarkDeliveryDay.isDefined, "Unassigned sale requires a benchmark delivery day")

  def instrumentType = UnallocatedSalesQuota

  def tradeableType = UnallocatedSalesQuota

  def asUtpPortfolio(tradeDay: Day) = UTP_Portfolio(Map(this -> 1.0))

  def *(scale: Double) = copy(quantity = quantity * scale)

  def isPurchase = false
}

object UnallocatedSalesQuota extends InstrumentType[UnallocatedSalesQuota] with TradeableType[UnallocatedSalesQuota] {

  import UOM._
  import PhysicalMetalAssignmentOrUnassignedSalesQuota._

  val name = "Unassigned Sales Quota"

  def createTradeable(row: RichInstrumentResultSetRow) = {
    val commodityName = row.getString(commodityLabel)
    val quantity = row.getQuantity(quantityLabel)
    val deliveryDay = row.getDay(contractDeliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
    val benchmarkDeliveryDay = Option(row.getDay(benchmarkPricingSpecLabel))
    val benchmarkPricingSpec = Option(row.getObject[TitanPricingSpec](benchmarkPricingSpecLabel))

    UnallocatedSalesQuota(commodityName, quantity, deliveryDay, pricingSpec, benchmarkDeliveryDay, benchmarkPricingSpec)
  }

  def sample = UnallocatedSalesQuota("Copper", Quantity(100, MT), Day(2012, 10, 12),
    AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD / MT)),
    Some(Day(2012, 11, 1)), 
    Some(AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 11), Quantity(1.0, USD/MT)))
  )
}

case class PhysicalMetalAssignment( assignmentID : String,
                                    commodityName: String,
                                    quantity: Quantity,
                                    contractDeliveryDay: Day,
                                    contractPricingSpec: TitanPricingSpec,
                                    benchmarkDeliveryDay : Option[Day],
                                    benchmarkPricingSpec: Option[TitanPricingSpec],
                                    isPurchase: Boolean,
                                    inventoryID: String,
                                    inventoryQuantity : Quantity
) extends PhysicalMetalAssignmentOrUnassignedSalesQuota with UTP with Tradeable {

  import PhysicalMetalAssignment._
  import PhysicalMetalAssignmentOrUnassignedSalesQuota._


  val weightGain = inventoryQuantity - quantity

  def instrumentType = PhysicalMetalAssignment

  def tradeableType = PhysicalMetalAssignment

  override def persistedTradeableDetails: Map[String, Any] = super[PhysicalMetalAssignmentOrUnassignedSalesQuota].persistedTradeableDetails ++ Map(
    inventoryIDLabel -> inventoryID, inventoryQuantityLabel -> inventoryQuantity, assignmentIDLabel -> assignmentID
  )

  override def shownTradeableDetails = super[PhysicalMetalAssignmentOrUnassignedSalesQuota].shownTradeableDetails ++ Map(assignmentIDLabel -> assignmentID, inventoryQuantityLabel -> inventoryQuantity, inventoryIDLabel -> inventoryID)

  def asUtpPortfolio(tradeDay: Day) = UTP_Portfolio(
    Map(this -> 1.0)
  )





  def *(scale: Double) = this.copy(quantity = quantity * scale, inventoryQuantity = inventoryQuantity * scale)


  def unallocatedPurchaseValue(env : Environment) : CostsAndIncomeUnallocatedAssignmentValuation = {
    require (isPurchase)
    benchmarkPricingSpec match {
      case Some(bp) => {
        CostsAndIncomeUnallocatedAssignmentValuation(
        assignmentID,
        /*
          Row 71
          Defined using the same rule as CostsAndIncomeAllocatedAssignmentValuation, however note that unallocated
          assignments only exist on the purchase side.
         */
        quantity,
        CostsAndIncomeValuation.buildEither(env, quantity, contractPricingSpec),
        CostsAndIncomeValuation.buildEither(env, quantity, bp),
        /*
          Rows 73, 75
          current inventory quantity - assignmentQuantity
         */
        weightGain,
        CostsAndIncomeValuation.buildEither(env, weightGain, bp),
        freightParity = Quantity.NULL)
      }
      case _ => throw new Exception("Missing benchmark pricing spec for unallocated purchase quota")
    }

/*
    require (!isAllocated && isPurchase)
    CostsAndIncomeUnallocatedAssignmentValuation(
      assignmentID,
      /*
        Row 71
        Defined using the same rule as CostsAndIncomeAllocatedAssignmentValuation, however note that unallocated
        assignments only exist on the purchase side.
       */
      quantity,
      CostsAndIncomeValuation.buildEither(env, quantity, pricingSpec),
      CostsAndIncomeValuation.buildEither(env, quantity, benchmarkPricingSpec),
      /*
        Rows 73, 75
        current inventory quantity - assignmentQuantity
       */
      weightGain,
      CostsAndIncomeValuation.buildEither(env, weightGain, benchmarkPricingSpec),
      freightParity = Quantity.NULL
    )
    */
  }

  def allocatedPurchaseValue(env : Environment) : CostsAndIncomeAllocatedPurchaseAssignmentValuation = {
    require(isAllocated && isPurchase)
    CostsAndIncomeAllocatedPurchaseAssignmentValuation(
      assignmentID,
      /*
         Rows 71, 72, 74
         For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
         For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
       */
      quantity,
      CostsAndIncomeValuation.buildEither(env, quantity, contractPricingSpec)
    )
  }

  def allocatedSaleValue(env : Environment) : CostsAndIncomeAllocatedSaleAssignmentValuation = {
    require (isAllocated && !isPurchase)
    CostsAndIncomeAllocatedSaleAssignmentValuation(
      assignmentID,
      /*
         Rows 71, 72, 74
         For a purchase assignment this is the final receipted quantity (when it exists) otherwise current inventory quantity
         For a salee assignment this is the final delivered quantity (when it exists) otherwise current inventory quantity
       */
      quantity,
      CostsAndIncomeValuation.buildEither(env, quantity, contractPricingSpec),
      weightGain,
      CostsAndIncomeValuation.buildEither(env, weightGain, contractPricingSpec)
    )
  }

}

object CostsAndIncomeValuation{
  def build(env : Environment, quantity : Quantity, pricingSpec : TitanPricingSpec) = {
    PricingValuationDetails(
      pricingSpec.price(env),
      pricingSpec.premium,
      pricingSpec.price(env) * quantity,
      pricingSpec.isComplete(env.marketDay),
      pricingSpec.fixedQuantity(env.marketDay, quantity),
      pricingSpec.pricingType,
      pricingSpec.quotationPeriod.map(_.firstDay),
      pricingSpec.quotationPeriod.map(_.lastDay),
      pricingSpec.indexOption.map(_.toString).getOrElse("Undefined")
    )
  }

  def buildEither(env : Environment, quantity : Quantity, pricingSpec : TitanPricingSpec) = {
    try {
      Right(build(env, quantity, pricingSpec))
    } catch {
      case e =>
        Left(e.getMessage)
    }
  }

}

object PhysicalMetalAssignmentOrUnassignedSalesQuota{
  val contractDeliveryDayLabel = "deliveryDay"
  val commodityLabel = "commodity"
  val quantityLabel = "Quantity"
  val contractPricingSpecNameLabel = "pricingSpecName"
  val contractPricingSpecLabel = "pricingSpec"
  val benchmarkPricingSpecLabel = "benchmarkPricingSpec"
  val benchmarkDeliveryDayLabel = "benchmarkDeliveryDay"
  val quotationPeriodLabel = "quotationPeriod"
  val premiumLabel = "premium"
  val exchangeLabel = "exchange"
  val indexLabel = "index"
  val isPurchaseLabel = "isPurchase"
}

object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment] {
    import PhysicalMetalAssignmentOrUnassignedSalesQuota._
    val inventoryQuantityLabel = "inventoryQuantity"
    val inventoryIDLabel = "inventoryID"
    val assignmentIDLabel = "assignmentID"

    val name = "Physical Metal Assignment"

    def createTradeable(row: RichInstrumentResultSetRow) = {
      val commodityName = row.getString(commodityLabel)
      val quantity = row.getQuantity(quantityLabel)
      val deliveryDay = row.getDay(contractDeliveryDayLabel)
      val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
      val benchmarkPricingSpec = Option(row.getObject[TitanPricingSpec](benchmarkPricingSpecLabel))
      val benchmarkDeliveryDay = Option(row.getDay(benchmarkDeliveryDayLabel))
      val isPurchase = row.getBoolean(isPurchaseLabel)
      val inventoryQuantity = row.getQuantity(inventoryQuantityLabel)
      val inventoryID = row.getString(inventoryIDLabel)
      val assignmentID = row.getString(assignmentIDLabel)

      PhysicalMetalAssignment(
        assignmentID,
        commodityName, quantity, deliveryDay, pricingSpec, benchmarkDeliveryDay, benchmarkPricingSpec, isPurchase, inventoryID, inventoryQuantity
      )
    }

    import UOM._
    def sample = PhysicalMetalAssignment(
      "12345", "Aluminium", Quantity(100, UOM.MT),
      Day(2011, 10, 10), AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD/MT)),
      Some(Day(2011, 11, 1)), Some(AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 11), Quantity(1, USD/MT))),
      isPurchase = false, inventoryID = "abcde", inventoryQuantity = Quantity(99, UOM.MT))
}
