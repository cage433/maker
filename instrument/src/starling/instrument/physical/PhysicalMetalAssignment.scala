package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.daterange._
import starling.instrument._
import starling.utils.sql.PersistAsBlob
import starling.market._
import starling.titan.valuation.{CostsAndIncomeAllocatedSaleAssignmentValuation, CostsAndIncomeAllocatedPurchaseAssignmentValuation, PricingValuationDetails, CostsAndIncomeUnallocatedAssignmentValuation}
import starling.quantity._
import starling.marketdata.{IncotermCode, GradeCode, ContractualLocationCode, NeptuneCountryCode}
import starling.market.{Commodity, NeptuneCommodity}
import starling.market.Copper
import starling.market.Aluminium
import starling.utils.Log
import starling.titan.valuation.AssignmentValuation
import starling.quantity.UOM._
import starling.curves.{MissingMarketDataException, Environment}


object PhysicalMetalAssignmentOrUnassignedSalesQuota{
  val contractDeliveryDayLabel = "contractDeliveryDay"
  val quantityLabel = "Quantity"
  val commodityLabel = "Commodity"
  val contractPricingSpecNameLabel = "contractPricingSpecName"
  val contractPricingSpecLabel = "contractPricingSpec"
  val contractLocationCodeLabel = "contractLocationCode"
  val contractIncoTermCodeLabel = "contractIncoTermCode"
  val benchmarkPricingSpecNameLabel = "benchmarkPricingSpecName"
  val benchmarkDeliveryDayLabel = "benchmarkDeliveryDay"
  val benchmarkCountryCodeLabel = "benchmarkCountryCode"
  val benchmarkIncoTermCodeLabel = "benchmarkIncoTermCode"
  val premiumLabel = "premium"
  val exchangeLabel = "exchange"
  val contractIndexLabel = "contractIndex"
  val benchmarkIndexLabel = "benchmarkIndex"
  val isPurchaseLabel = "isPurchase"
  val directionLabel = "direction"
  val gradeCodeLabel = "gradeCode"

  def detailsFromRow(row: RichInstrumentResultSetRow) = new PhysicalMetalAssignmentOrUnassignedSalesQuota(){
    val quantity = row.getQuantity(quantityLabel)
    val commodity= Commodity.neptuneCommodityFromNeptuneCode(row.getString(commodityLabel))
    val deliveryDay = row.getDay(contractDeliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](contractPricingSpecLabel)
    val contractLocationCode = ContractualLocationCode(row.getString(contractLocationCodeLabel))
    val contractIncoTermCode = IncotermCode(row.getString(contractIncoTermCodeLabel))
    val benchmarkDeliveryDay = if (!row.isNull(benchmarkDeliveryDayLabel)) Some(row.getDay(benchmarkDeliveryDayLabel)) else None
    val benchmarkCountryCode = (if (!row.isNull(benchmarkCountryCodeLabel)) Some(row.getString(benchmarkCountryCodeLabel)) else None).map(NeptuneCountryCode)
    val benchmarkIncoTermCode = (if (!row.isNull(benchmarkIncoTermCodeLabel)) Some(row.getString(benchmarkIncoTermCodeLabel)) else None).map(IncotermCode)
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
  def commodity : NeptuneCommodity
  def contractDeliveryDay: Day
  def contractPricingSpec: TitanPricingSpec
  def contractLocationCode : ContractualLocationCode
  def contractIncoTermCode : IncotermCode
  def benchmarkDeliveryDay : Option[Day]
  def benchmarkCountryCode : Option[NeptuneCountryCode]
  def benchmarkIncoTermCode : Option[IncotermCode]
  def grade : GradeCode
  def isPurchase : Boolean
  def valuationCurrency : UOM = contractPricingSpec.valuationCCY

  def inventoryQuantity = quantity // default to "quantity" but expect overridden value from inventory for allocations

  import PhysicalMetalAssignmentOrUnassignedSalesQuota._

  override def shownTradeableDetails = Map(
      commodityLabel -> commodity.neptuneName,
      contractDeliveryDayLabel -> contractDeliveryDay,
      contractPricingSpecNameLabel -> contractPricingSpec.pricingType,
      contractLocationCodeLabel -> contractLocationCode.code,
      contractIncoTermCodeLabel -> contractIncoTermCode.code,
      quantityLabel -> quantity,
      premiumLabel -> contractPricingSpec.premium,
      gradeCodeLabel -> grade.code,
      directionLabel -> (if (isPurchase) "P" else "S")
    ) ++ contractPricingSpec.indexOption.map(contractIndexLabel -> _) ++
      contractPricingSpec.indexOption.flatMap(_.market.exchangeOption).map(exchangeLabel -> _.name) ++
      benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _) ++
      benchmarkCountryCode.map(benchmarkCountryCodeLabel -> _.code) ++
      benchmarkIncoTermCode.map(benchmarkIncoTermCodeLabel -> _.code)

  def valuationCCY = contractPricingSpec.valuationCCY

  def persistedTradeableDetails: Map[String, Any] = Map(
    contractDeliveryDayLabel -> contractDeliveryDay,
    quantityLabel -> quantity,
    contractPricingSpecLabel -> PersistAsBlob(contractPricingSpec),
    isPurchaseLabel -> isPurchase,
    commodityLabel -> commodity.neptuneCode,
    contractLocationCodeLabel -> contractLocationCode.code,
    contractIncoTermCodeLabel -> contractIncoTermCode.code,
    gradeCodeLabel -> grade.code
  ) ++ benchmarkDeliveryDay.map(benchmarkDeliveryDayLabel -> _)  ++
        benchmarkCountryCode.map(benchmarkCountryCodeLabel -> _.code) ++
       benchmarkIncoTermCode.map(benchmarkIncoTermCodeLabel -> _.code)

  def price(env: Environment) = contractPricingSpec.priceExcludingVAT(env)

  def periodKey = contractPricingSpec.quotationPeriod.map(DateRangePeriod)

  def volume = quantity
  def isAllocated = ! benchmarkDeliveryDay.isDefined
  def daysForPositionReport(marketDay: DayAndTime) = contractPricingSpec.daysForPositionReport(marketDay)
  override def expiryDay() = Some(benchmarkDeliveryDay.map(_.containingMonth.lastDay).foldLeft(contractPricingSpec.expiryDay)(_ max _))

  private def discount(env: Environment, spec : TitanPricingSpec) = env.discount(valuationCCY, spec.settlementDay(env.marketDay)).named("Discount")


  private def timesVolume(price : NamedQuantity, qty : Quantity) : NamedQuantity = {
    val volume = qty.named("Volume")
    if (quantity.uom != price.denominatorUOM){
      val volumeConversion = quantity.inUOM(price.denominatorUOM) / quantity
      price * volumeConversion.named("Volume Conversion") * volume
    } else {
      price * volume
    }
  }

  private def discounted(env : Environment, payment : NamedQuantity, settlementDay : Day) : NamedQuantity = {
    val d = env.discount(payment.uom, settlementDay).named("Discount")
    if (payment.uom == valuationCCY)
      payment * d
    else {
      val fx = env.spotFXRate(valuationCCY, payment.uom).named("FX")
      payment * fx * d
    }
  }
  
  private def contractPaymentExplained(env : Environment) : NamedQuantity = {
    val price = contractPricingSpec.priceExcludingVAT(env)
    var exp : NamedQuantity = (if (isPurchase) price * -1 else price).named("Contract Price")
    exp = timesVolume(exp, quantity)
    discounted(env, exp, contractPricingSpec.settlementDay(env.marketDay))
  }

  private def benchmarkPaymentExplained(env : Environment) : NamedQuantity = {
    val spec = benchmarkPricingSpec(env)
    val price = spec.priceExcludingVAT(env)
    var exp : NamedQuantity = (if (isPurchase) price else price * -1).named("Benchmark Price")
    exp = timesVolume(exp, inventoryQuantity)
    discounted(env, exp, spec.settlementDay(env.marketDay))
  }

  def benchmarkPricingSpec(env : Environment) : TitanPricingSpec = {
    val month = benchmarkDeliveryDay.get.containingMonth
    val futuresMarket = contractPricingSpec.futuresMarket
    val index = futuresMarket.physicalMetalBenchmarkIndex
    val benchmarkLookupDay = TitanPricingSpec.representativeDay(index, month, env.marketDay)
    val benchmark = env.benchmarkPremium(commodity, benchmarkCountryCode.get, grade, benchmarkLookupDay)
    UnknownPricingSpecification(
      index,
      month,
      Nil,
      month.lastDay,
      benchmark,
      valuationCCY
    )
  }

  //private def freightParityExplained(env : Environment) : NamedQuantity = {
    //var exp : NamedQuantity = (env.freightParity(
      //contractIncoTermCode, contractLocationCode, 
      //benchmarkIncoTermCode.get, benchmarkCountryCode.get
      //) * -1).named("Freight Parity")
    //exp = timesVolume(exp)
    //discounted(env, exp, contractPricingSpec.settlementDay(env.marketDay))
    //}

  def explanation(env: Environment): NamedQuantity = {
    val namedEnv = env.withNaming()
    val components = assets(namedEnv).assets.map(_.mtm.asInstanceOf[NamedQuantity])
    components.tail.fold(components.head)(_+_)
  }

  def assets(env: Environment) = {
    val contractPaymentAsset = Asset(
      known = true,
      assetType = commodity.neptuneName,
      settlementDay = contractPricingSpec.settlementDay(env.marketDay),
      amount = quantity,
      mtm = contractPaymentExplained(env)
    )
    val assets = if (benchmarkDeliveryDay.isDefined){
      val benchmarkPaymentAsset = Asset(
        known = false,
        assetType = commodity.neptuneName,
        settlementDay = benchmarkPricingSpec(env).settlementDay(env.marketDay),
        amount = inventoryQuantity,
        mtm = benchmarkPaymentExplained(env)
      )
    //val freightParityAsset = Asset(
      //known = false,
      //assetType = "Freight",
      //settlementDay = benchmarkPricingSpec(env).settlementDay(env.marketDay),
      //amount = quantity,
      //mtm = freightParityExplained(env)
      //)
      List(contractPaymentAsset, benchmarkPaymentAsset)//, freightParityAsset)

    } else {
      List(contractPaymentAsset)
    }

    Assets(assets)
  }
}


case class UnallocatedSalesQuota(
                                  quantity: Quantity,
                                  commodity : NeptuneCommodity,
                                  contractDeliveryDay: Day,
                                  contractPricingSpec: TitanPricingSpec,
                                  contractLocationCode: ContractualLocationCode,
                                  contractIncoTermCode : IncotermCode,
                                  benchmarkDeliveryDay: Option[Day],
                                  benchmarkCountryCode: Option[NeptuneCountryCode],
                                  benchmarkIncoTermCode : Option[IncotermCode],
                                  grade: GradeCode
                                  ) extends PhysicalMetalAssignmentOrUnassignedSalesQuota with UTP with Tradeable {

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


    UnallocatedSalesQuota(quantity, commodity,
      deliveryDay, pricingSpec, contractLocationCode, contractIncoTermCode,
      benchmarkDeliveryDay, benchmarkCountryCode, benchmarkIncoTermCode,
      grade
    )
  }

  def sample = UnallocatedSalesQuota(Quantity(100, MT), Copper, Day(2012, 10, 12),
    AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD / MT), USD),
    ContractualLocationCode("France"), IncotermCode("CIF"),
    Some(Day(2012, 11, 1)), Some(NeptuneCountryCode("Italy")), Some(IncotermCode("CFR")),
    grade = GradeCode("grade")
  )
}

case class PhysicalMetalAssignment( assignmentID : String,
                                    quantity: Quantity,
                                    commodity : NeptuneCommodity,
                                    contractDeliveryDay: Day,
                                    contractPricingSpec: TitanPricingSpec,
                                    contractLocationCode : ContractualLocationCode,
                                    contractIncoTermCode : IncotermCode,
                                    benchmarkDeliveryDay : Option[Day],
                                    benchmarkCountryCode : Option[NeptuneCountryCode],
                                    benchmarkIncoTermCode : Option[IncotermCode],
                                    isPurchase: Boolean,
                                    inventoryID: String,
                                    override val inventoryQuantity : Quantity,
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

  def costsAndIncomeValuation(env : Environment) : AssignmentValuation = {
    if (isPurchase & isAllocated)
      allocatedPurchaseValue(env)
    else if (isPurchase)
      unallocatedPurchaseValue(env)
    else
      allocatedSaleValue(env)
  }

      
  def unallocatedPurchaseValue(env : Environment) : CostsAndIncomeUnallocatedAssignmentValuation = {
    require (isPurchase)
    benchmarkDeliveryDay match {
      case Some(bdd) => {
        val bp = benchmarkPricingSpec(env)
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
          freightParity = None)
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
        commodity,
        deliveryDay, pricingSpec,
        contractLocationCode,
        contractIncoTermCode,
        benchmarkDeliveryDay, benchmarkCountryCode, benchmarkIncoTermCode,
        isPurchase, inventoryID, inventoryQuantity,
        grade
      )
    }

    import UOM._
    def sample = PhysicalMetalAssignment(
      "12345", Quantity(100, UOM.MT), Aluminium,
      Day(2011, 10, 10), AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity(0.5, USD/MT), USD),
      ContractualLocationCode("France"), IncotermCode("CIF"),
      Some(Day(2011, 11, 1)), Some(NeptuneCountryCode("Italy")), Some(IncotermCode("CIF")),
      isPurchase = false, inventoryID = "abcde", inventoryQuantity = Quantity(99, UOM.MT),
      grade = GradeCode("grade")
    )
}



object CostsAndIncomeValuation{
  def build(env : Environment, quantity : Quantity, pricingSpec : TitanPricingSpec) = {
    // C&I asked that premiums equal to null quantity - which in TM means no premium
    // are represented by None
    val premium = if (pricingSpec.premium == Quantity.NULL)
      None
    else
      Some(pricingSpec.premium)

    PricingValuationDetails(
      pricingSpec.priceExcludingVAT(env),
      pricingSpec.priceIncludingVAT(env),
      premium,
      pricingSpec.priceExcludingVAT(env) * quantity,
      pricingSpec.priceIncludingVAT(env).map(_ * quantity),
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
      case e : MissingMarketDataException =>
        Left(e.getMessage)
    }
  }
}
