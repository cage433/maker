package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.utils.sql.PersistAsBlob
import starling.market._
import starling.titan.valuation.{CostsAndIncomeAllocatedSaleAssignmentValuation, CostsAndIncomeAllocatedPurchaseAssignmentValuation, PricingValuationDetails, CostsAndIncomeUnallocatedAssignmentValuation}
import starling.quantity._
import starling.marketdata.{GradeCode, ContractualLocationCode, NeptuneCountryCode}

object PhysicalMetalAssignmentOrUnassignedSalesQuota{
  val contractDeliveryDayLabel = "contractDeliveryDay"
  val quantityLabel = "Quantity"
  val commodityLabel = "Commodity"
  val contractPricingSpecNameLabel = "contractPricingSpecName"
  val contractPricingSpecLabel = "contractPricingSpec"
  val contractLocationCodeLabel = "contractLocationCode"
  val benchmarkPricingSpecNameLabel = "benchmarkPricingSpecName"
  val benchmarkDeliveryDayLabel = "benchmarkDeliveryDay"
  val benchmarkCountryCodeLabel = "benchmarkCountryCode"
  val premiumLabel = "premium"
  val exchangeLabel = "exchange"
  val contractIndexLabel = "contractIndex"
  val benchmarkIndexLabel = "benchmarkIndex"
  val isPurchaseLabel = "isPurchase"
  val directionLabel = "direction"
  val gradeCodeLabel = "gradeCode"

  def detailsFromRow(row: RichInstrumentResultSetRow) = new PhysicalMetalAssignmentOrUnassignedSalesQuota(){
    val quantity = row.getQuantity(quantityLabel)
    val commodityName = row.getString(commodityLabel)
    val deliveryDay = row.getDay(contractDeliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
    val contractLocationCode = ContractualLocationCode(row.getString(contractLocationCodeLabel))
    val benchmarkDeliveryDay = if (!row.isNull(benchmarkDeliveryDayLabel)) Some(row.getDay(benchmarkDeliveryDayLabel)) else None
    val benchmarkCountryCode = (if (!row.isNull(benchmarkCountryCodeLabel)) Some(row.getString(benchmarkCountryCodeLabel)) else None).map(NeptuneCountryCode)
    val grade = GradeCode(row.getString(gradeCodeLabel))
    val isPurchase = row.getBoolean(isPurchaseLabel)

    def instrumentType = throw new UnsupportedOperationException
    def tradeableType = throw new UnsupportedOperationException
    def asUtpPortfolio(tradeDay: Day) = throw new UnsupportedOperationException
    def *(scale: Double) = throw new UnsupportedOperationException
    def contractDeliveryDay = throw new UnsupportedOperationException
    def contractPricingSpec = throw new UnsupportedOperationException
  }


}

trait PhysicalMetalAssignmentOrUnassignedSalesQuota extends UTP with Tradeable {
  def quantity: Quantity
  def commodityName : String
  def contractDeliveryDay: Day
  def contractPricingSpec: TitanPricingSpec
  def contractLocationCode : ContractualLocationCode
  def benchmarkDeliveryDay : Option[Day]
  def benchmarkCountryCode : Option[NeptuneCountryCode]
  def grade : GradeCode
  def isPurchase : Boolean


  lazy val benchmarkPricingSpec = benchmarkDeliveryDay.map(_ => contractPricingSpec.dummyTransferPricingSpec)


  import PhysicalMetalAssignmentOrUnassignedSalesQuota._

  override def shownTradeableDetails = Map(
      commodityLabel -> commodityName,
      contractDeliveryDayLabel -> contractDeliveryDay,
      contractPricingSpecNameLabel -> contractPricingSpec.pricingType,
      contractLocationCodeLabel -> contractLocationCode.code,
      quantityLabel -> quantity,
      premiumLabel -> contractPricingSpec.premium,
      gradeCodeLabel -> grade.code,
      directionLabel -> (if (isPurchase) "P" else "S")
    ) ++ contractPricingSpec.indexOption.map(contractIndexLabel -> _) ++
      contractPricingSpec.indexOption.flatMap(_.market.exchangeOption).map(exchangeLabel -> _.name) ++
      benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _) ++
      benchmarkCountryCode.map(benchmarkCountryCodeLabel -> _.code)

  def isLive(dayAndTime: DayAndTime) = !contractPricingSpec.isComplete(dayAndTime) && (benchmarkPricingSpec match {
    case Some(spec) => !spec.isComplete(dayAndTime)
    case None => true
  })

  def valuationCCY = contractPricingSpec.valuationCCY

  def persistedTradeableDetails: Map[String, Any] = Map(
    contractDeliveryDayLabel -> contractDeliveryDay,
    quantityLabel -> quantity,
    contractPricingSpecLabel -> PersistAsBlob(contractPricingSpec),
    isPurchaseLabel -> isPurchase,
    commodityLabel -> commodityName,
    contractLocationCodeLabel -> contractLocationCode.code,
    gradeCodeLabel -> grade.code
  ) ++ benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _)  ++
        benchmarkCountryCode.map(benchmarkCountryCodeLabel -> _.code)

  def price(env: Environment) = contractPricingSpec.price(env)

  def periodKey = contractPricingSpec.quotationPeriod.map(DateRangePeriod)

  def volume = quantity
  def isAllocated = ! benchmarkPricingSpec.isDefined
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
                                  contractLocationCode: ContractualLocationCode,
                                  benchmarkDeliveryDay: Option[Day],
                                  benchmarkCountryCode: Option[NeptuneCountryCode],
                                  grade: GradeCode
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
    val details = detailsFromRow(row)
    import details._


    UnallocatedSalesQuota(quantity, commodityName,
      deliveryDay, pricingSpec, contractLocationCode,
      benchmarkDeliveryDay, benchmarkCountryCode,
      grade
    )
  }

  def sample = UnallocatedSalesQuota(Quantity(100, MT), "Copper", Day(2012, 10, 12),
    AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD / MT)), ContractualLocationCode("France"),
    Some(Day(2012, 11, 1)), Some(NeptuneCountryCode("Italy")),
    grade = GradeCode("grade")
  )
}

case class PhysicalMetalAssignment( assignmentID : String,
                                    quantity: Quantity,
                                    commodityName : String,
                                    contractDeliveryDay: Day,
                                    contractPricingSpec: TitanPricingSpec,
                                    contractLocationCode : ContractualLocationCode,
                                    benchmarkDeliveryDay : Option[Day],
                                    benchmarkCountryCode : Option[NeptuneCountryCode],
                                    isPurchase: Boolean,
                                    inventoryID: String,
                                    inventoryQuantity : Quantity,
                                    grade : GradeCode
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



object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment] {
    import PhysicalMetalAssignmentOrUnassignedSalesQuota._
    val inventoryQuantityLabel = "inventoryQuantity"
    val inventoryIDLabel = "inventoryID"
    val assignmentIDLabel = "assignmentID"

    val name = "Physical Metal Assignment"

    def createTradeable(row: RichInstrumentResultSetRow) = {
      val commonDetails = detailsFromRow(row)
      import commonDetails._
      val inventoryQuantity = row.getQuantity(inventoryQuantityLabel)
      val inventoryID = row.getString(inventoryIDLabel)
      val assignmentID = row.getString(assignmentIDLabel)

      PhysicalMetalAssignment(
        assignmentID,
        quantity,
        commodityName,
        deliveryDay, pricingSpec, contractLocationCode,
        benchmarkDeliveryDay, benchmarkCountryCode,
        isPurchase, inventoryID, inventoryQuantity,
        grade
      )
    }

    import UOM._
    def sample = PhysicalMetalAssignment(
      "12345", Quantity(100, UOM.MT), "Aluminium",
      Day(2011, 10, 10), AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD/MT)), ContractualLocationCode("France"),
      Some(Day(2011, 11, 1)), Some(NeptuneCountryCode("Italy")),
      isPurchase = false, inventoryID = "abcde", inventoryQuantity = Quantity(99, UOM.MT),
      grade = GradeCode("grade")
    )
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
