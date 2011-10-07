package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.utils.sql.PersistAsBlob
import starling.market._
import starling.titan.valuation.{CostsAndIncomeAllocatedSaleAssignmentValuation, CostsAndIncomeAllocatedPurchaseAssignmentValuation, PricingValuationDetails, CostsAndIncomeUnallocatedAssignmentValuation}
import starling.quantity._

trait TitanPricingSpec {

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

  def dummyTransferPricingSpec = copy(premium = premium.copy(value = 0))

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

  def dummyTransferPricingSpec = copy(premium = premium.copy(value = 0))

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


trait PhysicalMetalAssignmentOrUnassignedSalesQuota extends UTP with Tradeable {
  def quantity: Quantity
  def commodityName : String
  def contractDeliveryDay: Day
  def contractPricingSpec: TitanPricingSpec
  def contractDeliveryLocation : String
  def benchmarkDeliveryDay : Option[Day]
  def benchmarkDeliveryLocation : Option[String]
  def grade : String
  def isPurchase: Boolean

  val benchmarkPricingSpec = benchmarkDeliveryDay.map(_ => contractPricingSpec.dummyTransferPricingSpec)


  import PhysicalMetalAssignmentOrUnassignedSalesQuota._

  override def shownTradeableDetails = Map(
      commodityLabel -> commodityName,
      contractDeliveryDayLabel -> contractDeliveryDay,
      contractPricingSpecNameLabel -> contractPricingSpec.pricingType,
      contractDeliveryLocationLabel -> contractDeliveryLocation,
      quantityLabel -> quantity,
      premiumLabel -> contractPricingSpec.premium,
      gradeLabel -> grade,
      directionLabel -> (if (isPurchase) "P" else "S")
    ) ++ contractPricingSpec.indexOption.map(contractIndexLabel -> _) ++
      contractPricingSpec.indexOption.flatMap(_.market.exchangeOption).map(exchangeLabel -> _.name) ++
      benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _) ++
      benchmarkDeliveryLocation.map(benchmarkDeliveryLocationLabel -> _)

  def isLive(dayAndTime: DayAndTime) = !contractPricingSpec.isComplete(dayAndTime) && (benchmarkPricingSpec match {
    case Some(spec) => !spec.isComplete(dayAndTime)
    case None => true
  })

  def valuationCCY = contractPricingSpec.valuationCCY

  def persistedTradeableDetails: Map[String, Any] = Map(
    contractDeliveryDayLabel -> contractDeliveryDay, quantityLabel -> quantity,
    contractPricingSpecLabel -> PersistAsBlob(contractPricingSpec), isPurchaseLabel -> isPurchase,
    commodityLabel -> commodityName, contractDeliveryLocationLabel -> contractDeliveryLocation,
    gradeLabel -> grade
  ) ++ benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _)  ++
        benchmarkDeliveryLocation.map(benchmarkDeliveryLocationLabel -> _)

  def price(env: Environment) = contractPricingSpec.price(env)

  def periodKey = contractPricingSpec.quotationPeriod.map(DateRangePeriod)

  def volume = quantity
  def isAllocated = ! benchmarkPricingSpec.isDefined
  def detailsForUTPNOTUSED: Map[String, Any] = shownTradeableDetails
  def daysForPositionReport(marketDay: DayAndTime) = contractPricingSpec.daysForPositionReport(marketDay)
  override def expiryDay() = Some(contractPricingSpec.expiryDay)

  private def discount(env: Environment, spec : TitanPricingSpec) = env.discount(valuationCCY, spec.settlementDay(env.marketDay)).named("Discount")

  private def pricingSpecPaymentExplained(env : Environment, spec : TitanPricingSpec) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val isContractSpec = spec == contractPricingSpec
    val priceName = if (isContractSpec) "Contract Price" else "Benchmark Price"
    val price = spec.price(namedEnv).named(priceName)
    val quantityInMarketUOM = quantity.inUOM(price.denominatorUOM).named("Volume")
    val d = discount(env, contractPricingSpec)

    price * quantityInMarketUOM * d
  }

  def explanation(env: Environment): NamedQuantity = {
    var exp = pricingSpecPaymentExplained(env, contractPricingSpec)
    if (benchmarkPricingSpec.isDefined)
      exp = exp - pricingSpecPaymentExplained(env, benchmarkPricingSpec.get)
    if (isPurchase)
      exp = -exp
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


case class UnallocatedSalesQuota(
                                  quantity: Quantity,
                                  commodityName : String,
                                  contractDeliveryDay: Day,
                                  contractPricingSpec: TitanPricingSpec,
                                  contractDeliveryLocation: String,
                                  benchmarkDeliveryDay: Option[Day],
                                  benchmarkDeliveryLocation: Option[String],
                                  grade: String
                                  ) extends PhysicalMetalAssignmentOrUnassignedSalesQuota with UTP with Tradeable {

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
    val quantity = row.getQuantity(quantityLabel)
    val commodityName = row.getString(commodityLabel)
    val deliveryDay = row.getDay(contractDeliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
    val contractDeliveryLocation = row.getString(contractDeliveryLocationLabel)
    val benchmarkDeliveryDay = Option(row.getDay(benchmarkDeliveryDayLabel))
    val benchmarkDeliveryLocation = Option(row.getString(benchmarkDeliveryLocationLabel))


    UnallocatedSalesQuota(quantity, commodityName,
      deliveryDay, pricingSpec, contractDeliveryLocation,
      benchmarkDeliveryDay, benchmarkDeliveryLocation,
      grade = "grade"
    )
  }

  def sample = UnallocatedSalesQuota(Quantity(100, MT), "Copper", Day(2012, 10, 12),
    AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD / MT)), "France",
    Some(Day(2012, 11, 1)), Some("Italy"),
    grade = "grade"
  )
}

case class PhysicalMetalAssignment( assignmentID : String,
                                    quantity: Quantity,
                                    commodityName : String,
                                    contractDeliveryDay: Day,
                                    contractPricingSpec: TitanPricingSpec,
                                    contractDeliveryLocation : String,
                                    benchmarkDeliveryDay : Option[Day],
                                    benchmarkDeliveryLocation : Option[String],
                                    isPurchase: Boolean,
                                    inventoryID: String,
                                    inventoryQuantity : Quantity,
                                    grade : String
                                    ) extends PhysicalMetalAssignmentOrUnassignedSalesQuota with UTP with Tradeable {

  import PhysicalMetalAssignment._

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
  val contractDeliveryDayLabel = "contractDeliveryDay"
  val quantityLabel = "Quantity"
  val commodityLabel = "Commodity"
  val contractPricingSpecNameLabel = "contractPricingSpecName"
  val contractPricingSpecLabel = "contractPricingSpec"
  val contractDeliveryLocationLabel = "contractDeliveryLocation"
  val benchmarkPricingSpecNameLabel = "benchmarkPricingSpecName"
  val benchmarkDeliveryDayLabel = "benchmarkDeliveryDay"
  val benchmarkDeliveryLocationLabel = "benchmarkDeliveryLocation"
  val premiumLabel = "premium"
  val exchangeLabel = "exchange"
  val contractIndexLabel = "contractIndex"
  val benchmarkIndexLabel = "benchmarkIndex"
  val isPurchaseLabel = "isPurchase"
  val directionLabel = "direction"
  val gradeLabel = "grade"
}

object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment] {
    import PhysicalMetalAssignmentOrUnassignedSalesQuota._
    val inventoryQuantityLabel = "inventoryQuantity"
    val inventoryIDLabel = "inventoryID"
    val assignmentIDLabel = "assignmentID"

    val name = "Physical Metal Assignment"

    def createTradeable(row: RichInstrumentResultSetRow) = {
      val quantity = row.getQuantity(quantityLabel)
      val commodityName = row.getString(commodityLabel)
      val deliveryDay = row.getDay(contractDeliveryDayLabel)
      val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
      val contractDeliveryLocation = row.getString(contractDeliveryLocationLabel)
      val benchmarkDeliveryDay = if (!row.isNull(benchmarkDeliveryDayLabel)) Some(row.getDay(benchmarkDeliveryDayLabel)) else None
      val benchmarkDeliveryLocation = if (!row.isNull(benchmarkDeliveryLocationLabel)) Some(row.getString(benchmarkDeliveryLocationLabel)) else None
      val isPurchase = row.getBoolean(isPurchaseLabel)
      val inventoryQuantity = row.getQuantity(inventoryQuantityLabel)
      val inventoryID = row.getString(inventoryIDLabel)
      val assignmentID = row.getString(assignmentIDLabel)
      val grade = row.getString(gradeLabel)

      PhysicalMetalAssignment(
        assignmentID,
        quantity,
        commodityName,
        deliveryDay, pricingSpec, contractDeliveryLocation,
        benchmarkDeliveryDay, benchmarkDeliveryLocation,
        isPurchase, inventoryID, inventoryQuantity,
        grade
      )
    }

    import UOM._
    def sample = PhysicalMetalAssignment(
      "12345", Quantity(100, UOM.MT), "Aluminium",
      Day(2011, 10, 10), AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD/MT)), "France",
      Some(Day(2011, 11, 1)), Some("Italy"),
      isPurchase = false, inventoryID = "abcde", inventoryQuantity = Quantity(99, UOM.MT),
      grade = "grade"
    )
}
