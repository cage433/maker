package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.quantity.{UOM, Quantity}
import starling.utils.sql.PersistAsBlob
import starling.quantity.NamedQuantity
import starling.market._

trait TitanPricingSpec {
  import TitanPricingSpec._
  def price(env : Environment) : Quantity
  def settlementDay(marketDay : DayAndTime) : Day

  def premiumCCY : Option[UOM] = {
    premium.uom.numeratorUOM match {
      case UOM.NULL => None
      case ccy => Some(ccy)
    }
  }
  def valuationCCY : UOM
  
  def addPremiumConvertingIfNecessary(env : Environment, price : Quantity, premium : Quantity) : Quantity = {
    if (premium == Quantity.NULL)
      price
    else {
      val premiumCurrency = premium.uom.numeratorUOM
      val priceCurrency = price.uom.numeratorUOM
      price * env.forwardFXRate(premiumCurrency, priceCurrency, settlementDay(env.marketDay)) + premium.named("Premium")
    }
  }

  // This will go once EDM trades have both pricing specs
  def dummyTransferPricingSpec : TitanPricingSpec
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity
  def isComplete(marketDay : DayAndTime) : Boolean
  def pricingType : String
  def daysForPositionReport(marketDay : DayAndTime) : List[Day]
  def quotationPeriod : Option[DateRange]
  def premium : Quantity
  def indexOption : Option[IndexWithDailyPrices]
  def expiryDay : Day
}

object TitanPricingSpec{
  def calcSettlementDay(index : IndexWithDailyPrices, day : Day) = {
    val lme = FuturesExchangeFactory.LME
    val market: CommodityMarket = index.market
    market.asInstanceOf[FuturesMarket].exchange match {
      case `lme` => day.addBusinessDays(market.businessCalendar, 2)
      case _ => day
    }
  }
}

case class AveragePricingSpec(index : IndexWithDailyPrices, period : DateRange, premium : Quantity) extends TitanPricingSpec{
  val observationDays = index.observationDays(period)
  // Just a guess
  def settlementDay(marketDay : DayAndTime) = TitanPricingSpec.calcSettlementDay(index, period.lastDay)
  def price(env: Environment) = addPremiumConvertingIfNecessary(env, env.averagePrice(index, period), premium)
  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity = totalQuantity * observedDays(marketDay).size / observationDays.size
  def pricingType : String = "Month Average"

  protected def observedDays(marketDay : DayAndTime) = observationDays.filter(_.endOfDay <= marketDay)
  def isComplete(marketDay : DayAndTime) = observedDays(marketDay).size == observationDays.size
  def indexOption = Some(index)
  def quotationPeriod = Some(period)
  def daysForPositionReport(marketDay: DayAndTime) = observationDays.filter(_.endOfDay > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)
  def expiryDay = TitanPricingSpec.calcSettlementDay(index, period.lastDay)
}


case class OptionalPricingSpec(choices : List[TitanPricingSpec], declarationDay : Day, chosenSpec : Option[TitanPricingSpec]) extends TitanPricingSpec {

  private val specToUse = chosenSpec.getOrElse(choices.head)
  def settlementDay(marketDay : DayAndTime) = specToUse.settlementDay(marketDay)
  def price(env: Environment) = {
    assert(chosenSpec.isDefined || env.marketDay < declarationDay.endOfDay, "Optional pricing spec must be fixed by " + declarationDay)
    specToUse.price(env)
  }

  def dummyTransferPricingSpec = OptionalPricingSpec(choices.map(_.dummyTransferPricingSpec), declarationDay, chosenSpec.map(_.dummyTransferPricingSpec))
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = specToUse.fixedQuantity(marketDay, totalQuantity)
  def isComplete(marketDay : DayAndTime) = specToUse.isComplete(marketDay)
  def pricingType : String = chosenSpec match {case Some(spec) => spec.pricingType; case None => "Optional"}
  def premium = specToUse.premium

  def daysForPositionReport(marketDay: DayAndTime) = specToUse.daysForPositionReport(marketDay)

  def valuationCCY = specToUse.valuationCCY
  def quotationPeriod = specToUse.quotationPeriod
  def indexOption = specToUse.indexOption

  def expiryDay = specToUse.expiryDay
}

case class WeightedPricingSpec(specs : List[(Double, TitanPricingSpec)]) extends TitanPricingSpec{
  def settlementDay(marketDay : DayAndTime) = specs.flatMap(_._2.settlementDay(marketDay)).sortWith(_>_).head
  def price(env: Environment) = Quantity.sum(specs.map{case (weight, spec) => spec.price(env) * weight})

  def dummyTransferPricingSpec = WeightedPricingSpec(specs.map{case (wt, spec) => (wt, spec.dummyTransferPricingSpec)})
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = specs.map{case (wt, spec) => spec.fixedQuantity(marketDay, totalQuantity) * wt}.sum
  def isComplete(marketDay : DayAndTime) = specs.forall{_._2.isComplete(marketDay)}
  def pricingType : String = "Weighted"

  private def quotationPeriodStart : Option[Day] = specs.map(_._2.quotationPeriod).collect{case Some(period) => period.firstDay}.sortWith(_<_).headOption
  private def quotationPeriodEnd : Option[Day] = specs.map(_._2.quotationPeriod).collect{case Some(period) => period.lastDay}.sortWith(_<_).lastOption
  def premium = specs.map{case (wt, spec) => spec.premium * wt}.sum

  def daysForPositionReport(marketDay: DayAndTime) = specs.flatMap{case (amt, spec) => spec.daysForPositionReport(marketDay) }.distinct

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
case class InvalidTitanPricingSpecException(msg : String) extends Exception(msg)

case class FixedPricingSpec (settDay : Day, pricesByFraction : List[(Double, Quantity)], premium : Quantity) extends TitanPricingSpec{

  def settlementDay(marketDay: DayAndTime) = settDay

  def price(env: Environment) = {
    val totalFraction = pricesByFraction.map(_._1).sum
    if (totalFraction == 0){
        throw new InvalidTitanPricingSpecException("Fixed Pricing Spec with no fixed prices")
    } else {
      val fixedPriceComponent = Quantity.sum(pricesByFraction.zipWithIndex.map{
        case ((qty, prc), i) => prc.named("F_" + i) * qty
      }) / totalFraction
      addPremiumConvertingIfNecessary(env, fixedPriceComponent, premium)
    }
  }

  def dummyTransferPricingSpec = copy()
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = totalQuantity
  def isComplete(marketDay : DayAndTime) = true
  def pricingType : String = "Fixed"
  def quotationPeriodStart : Option[Day] = None
  def quotationPeriodEnd : Option[Day] = None
  def indexName : String = "No Index"
  
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

case class UnknownPricingFixation(fraction : Double, price : Quantity)
case class UnknownPricingSpecification(
  index : IndexWithDailyPrices,
  month : Month,
  fixations : List[UnknownPricingFixation],
  declarationDay : Day,
  premium : Quantity
)
  extends TitanPricingSpec
{

  def settlementDay(marketDay : DayAndTime) = TitanPricingSpec.calcSettlementDay(index, unfixedPriceDay(marketDay))
  

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
    val fixedPayment = Quantity.sum(fixations.zipWithIndex.map{ case (f, i) => f.price.named("Fix_" + i) * f.fraction}).named("Fixed")
    val unfixedPayment = (index.fixingOrForwardPrice(env, unfixedPriceDay(env.marketDay)) * unfixedFraction).named("Unfixed")
    addPremiumConvertingIfNecessary(env, fixedPayment + unfixedPayment, premium)
  }

  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = totalQuantity * fixations.map(_.fraction).sum
  def isComplete(marketDay : DayAndTime) = declarationDay.endOfDay >= marketDay
  def pricingType : String = "Unknown"
  def quotationPeriodStart : Option[Day] = Some(month.firstDay)
  def quotationPeriodEnd : Option[Day] = Some(month.lastDay)
  def indexName : String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = index.observationDays(month).filter(_.endOfDay > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)
  def quotationPeriod = Some(month)
  def indexOption = Some(index)

  def expiryDay = TitanPricingSpec.calcSettlementDay(index, index.observationDays(month).last)
}

case class PhysicalMetalAssignment(
  commodityName : String, 
  quantity : Quantity, 
  deliveryDay : Day, 
  pricingSpec : TitanPricingSpec) extends UTP with Tradeable {
  import PhysicalMetalAssignment._

  def isLive(dayAndTime: DayAndTime) = !pricingSpec.isComplete(dayAndTime)

  def valuationCCY = pricingSpec.valuationCCY

  def detailsForUTPNOTUSED :Map[String, Any] = shownTradeableDetails

  def instrumentType = PhysicalMetalAssignment
  def tradeableType = PhysicalMetalAssignment

  def persistedTradeableDetails : Map[String, Any] = Map(commodityLabel -> commodityName, deliveryDayLabel -> deliveryDay, quantityLabel -> quantity, pricingSpecLabel -> PersistAsBlob(pricingSpec))
  override def shownTradeableDetails = Map(
    commodityLabel -> commodityName, 
    deliveryDayLabel -> deliveryDay, 
    pricingSpecNameLabel -> pricingSpec.pricingType, 
    quantityLabel -> quantity,
    premiumLabel -> pricingSpec.premium
  ) ++ pricingSpec.indexOption.map(indexLabel -> _) ++
    pricingSpec.quotationPeriod.map(quotationPeriodLabel -> _) ++
    pricingSpec.indexOption.flatMap(_.market.exchangeOption).map(exchangeLabel -> _.name)

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(copy(quantity = Quantity(1.0, quantity.uom)) -> quantity.value))

  private def cashMtm(env : Environment) : Quantity = cashMtmWithBreakdown(env)._1
  private def cashMtmWithBreakdown(env : Environment) : (Quantity, (NamedQuantity, NamedQuantity)) = {
    val price = pricingSpec.price(env).named("F")
    val quantityInMarketUOM = quantity.inUOM(price.denominatorUOM).named("Volume")
    (- (price * quantityInMarketUOM), (price, quantityInMarketUOM))
  }

  private def discount(env : Environment) = env.discount(valuationCCY, pricingSpec.settlementDay(env.marketDay)).named("Discount")
  def explanation(env : Environment) : NamedQuantity = {
    val namedEnv = env.withNaming()
    val (_, (price, volume)) = cashMtmWithBreakdown(namedEnv)
    (- price * volume) * discount(namedEnv)
  }

  def assets(env: Environment) = {
    val cashMtm_ = cashMtm(env)
    Assets(
      // Physical
      Asset(
        known = true,
        assetType = commodityName,
        settlementDay = deliveryDay,
        amount = quantity,
        mtm = Quantity(0, cashMtm_.uom.numeratorUOM)
      ),
      // Cash
      Asset(
        known = pricingSpec.isComplete(env.marketDay),
        assetType = commodityName,
        settlementDay = pricingSpec.settlementDay(env.marketDay),
        amount = quantity,
        mtm = cashMtm_ * discount(env)
      )
    )
  }

  def price(env: Environment) = pricingSpec.price(env)

  def periodKey = pricingSpec.quotationPeriod.map(DateRangePeriod)

  def *(scale: Double) = this.copy(quantity = quantity * scale)

  def daysForPositionReport(marketDay: DayAndTime) = pricingSpec.daysForPositionReport(marketDay)

  def volume = quantity

  override def expiryDay() = Some(pricingSpec.expiryDay)
}

object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment]{
  val deliveryDayLabel = "deliveryDay"
  val commodityLabel = "commodity"
  val quantityLabel = "Quantity"
  val pricingSpecNameLabel = "pricingSpecName"
  val pricingSpecLabel = "pricingSpec"
  val quotationPeriodLabel = "quotationPeriod"
  val premiumLabel = "premium"
  val exchangeLabel = "exchange"
  val indexLabel = "index"

  val name = "Physical Metal Assignment"
  
  def createTradeable(row: RichInstrumentResultSetRow) = {
    val commodityName = row.getString(commodityLabel)
    val quantity = row.getQuantity(quantityLabel)
    val deliveryDay = row.getDay(deliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](pricingSpecLabel)
    PhysicalMetalAssignment(commodityName, quantity, deliveryDay, pricingSpec)
  }
  def sample = PhysicalMetalAssignment("Aluminium", Quantity(100, UOM.MT), Day(2011, 10, 10), AveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity.NULL))
}
