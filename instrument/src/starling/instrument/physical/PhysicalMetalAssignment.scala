package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.quantity.{UOM, Quantity}
import starling.utils.sql.PersistAsBlob
import starling.market.{LmeSingleIndices, IndexWithKnownPrice}
import collection.SortedMap
import starling.quantity.NamedQuantity

trait TitanPricingSpec {
  def price(env : Environment) : Quantity
  def settlementDay : Day

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
      price * env.forwardFXRate(premiumCurrency, priceCurrency, settlementDay) + premium.named("Premium")
    }
  }

  // This will go once EDM trades have both pricing specs
  def dummyTransferPricingSpec : TitanPricingSpec
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity
  def isComplete(marketDay : DayAndTime) : Boolean
  def pricingType : String
  def quotationPeriodStart : Option[Day]
  def quotationPeriodEnd : Option[Day]
  def indexName : String
  def premium : Quantity
  def daysForPositionReport(marketDay : DayAndTime) : List[Day]
}


trait AveragePricingSpec extends TitanPricingSpec {
  def observationDays : List[Day]
  def index : IndexWithKnownPrice
  protected def observedDays(marketDay : DayAndTime) = observationDays.filter(_.endOfDay <= marketDay)
  def isComplete(marketDay : DayAndTime) = observedDays(marketDay).size == observationDays.size
  def quotationPeriodStart : Option[Day] = Some(observationDays.head)
  def quotationPeriodEnd : Option[Day] = Some(observationDays.last)
  def indexName : String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = observationDays.filter(_.endOfDay() > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)
}

case class MonthAveragePricingSpec(index : IndexWithKnownPrice, month : Month, premium : Quantity) extends AveragePricingSpec{
  val observationDays = index.observationDays(month)
  // Just a guess
  def settlementDay = month.lastDay.addBusinessDays(index.businessCalendar, 2)
  def price(env: Environment) = addPremiumConvertingIfNecessary(env, env.averagePrice(index, month), premium)
  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity = totalQuantity * observedDays(marketDay).size / observationDays.size
  def pricingType : String = "Month Average"
}

case class PartialAveragePricingSpec(index : IndexWithKnownPrice, dayFractions : SortedMap[Day, Double], premium : Quantity) extends AveragePricingSpec {

  val observationDays = dayFractions.keySet.toList.sortWith(_<_)
  def settlementDay = dayFractions.keys.toList.sortWith(_>_).head.addBusinessDays(index.businessCalendar, 2)

  def price(env: Environment) = {
    val priceFractions = dayFractions.map {
        case (day, frac) => index.fixingOrForwardPrice(env, day) * frac
      }
    addPremiumConvertingIfNecessary(
      env,
      Quantity.sum(priceFractions),
      premium
    )
  }

  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity = totalQuantity * observationDays.filter(_.endOfDay <= marketDay).map(dayFractions).sum
  def pricingType : String = "Partial Average"
}

case class OptionalPricingSpec(choices : List[TitanPricingSpec], declarationDay : Day, chosenSpec : Option[TitanPricingSpec]) extends TitanPricingSpec {

  private val specToUse = chosenSpec.getOrElse(choices.head)
  def settlementDay = choices.map(_.settlementDay).sortWith(_>_).head
  def price(env: Environment) = {
    assert(chosenSpec.isDefined || env.marketDay < declarationDay.endOfDay, "Optional pricing spec must be fixed by " + declarationDay)
    specToUse.price(env)
  }

  def dummyTransferPricingSpec = OptionalPricingSpec(choices.map(_.dummyTransferPricingSpec), declarationDay, chosenSpec.map(_.dummyTransferPricingSpec))
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = specToUse.fixedQuantity(marketDay, totalQuantity)
  def isComplete(marketDay : DayAndTime) = specToUse.isComplete(marketDay)
  def pricingType : String = chosenSpec match {case Some(spec) => spec.pricingType; case None => "Optional"}
  def quotationPeriodStart : Option[Day] = specToUse.quotationPeriodStart
  def quotationPeriodEnd : Option[Day] = specToUse.quotationPeriodEnd
  def indexName : String = specToUse.indexName
  def premium = specToUse.premium

  def daysForPositionReport(marketDay: DayAndTime) = specToUse.daysForPositionReport(marketDay)

  def valuationCCY = specToUse.valuationCCY
}

case class WeightedPricingSpec(specs : List[(Double, TitanPricingSpec)]) extends TitanPricingSpec{
  def settlementDay = specs.flatMap(_._2.settlementDay).sortWith(_>_).head
  def price(env: Environment) = Quantity.sum(specs.map{case (weight, spec) => spec.price(env) * weight})

  def dummyTransferPricingSpec = WeightedPricingSpec(specs.map{case (wt, spec) => (wt, spec.dummyTransferPricingSpec)})
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = specs.map{case (wt, spec) => spec.fixedQuantity(marketDay, totalQuantity) * wt}.sum
  def isComplete(marketDay : DayAndTime) = specs.forall{_._2.isComplete(marketDay)}
  def pricingType : String = "Weighted"
  def quotationPeriodStart : Option[Day] = specs.map(_._2.quotationPeriodStart).filter(_.isDefined).map(_.get).sortWith(_<_).headOption
  def quotationPeriodEnd : Option[Day] = specs.map(_._2.quotationPeriodEnd).filter(_.isDefined).map(_.get).sortWith(_<_).lastOption
  def indexName : String = specs.head._2.indexName
  def premium = specs.map{case (wt, spec) => spec.premium * wt}.sum

  def daysForPositionReport(marketDay: DayAndTime) = specs.flatMap{case (amt, spec) => spec.daysForPositionReport(marketDay) }.distinct

  def valuationCCY = {
    val ccys = specs.map(_._2.valuationCCY).distinct
    assert(ccys.size == 1, "No unique valuation currency")
    ccys.head
  }
}
case class InvalidTitanPricingSpecException(msg : String) extends Exception(msg)

case class FixedPricingSpec (settlementDay : Day, pricesByFraction : List[(Double, Quantity)]) extends TitanPricingSpec{

  def price(env: Environment) = {
    val totalFraction = pricesByFraction.map(_._1).sum
    if (totalFraction == 0){
        throw new InvalidTitanPricingSpecException("Fixed Pricing Spec with no fixed prices")
    } else {
      Quantity.sum(pricesByFraction.zipWithIndex.map{
        case ((qty, prc), i) => prc.named("F_" + i) * qty
      }) / totalFraction
    }
  }

  def dummyTransferPricingSpec = copy()
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = totalQuantity
  def isComplete(marketDay : DayAndTime) = true
  def pricingType : String = "Fixed"
  def quotationPeriodStart : Option[Day] = None
  def quotationPeriodEnd : Option[Day] = None
  def indexName : String = "No Index"
  def premium = Quantity.NULL

  def daysForPositionReport(marketDay: DayAndTime) = List(marketDay.day)

  def valuationCCY = {
    val ccys = pricesByFraction.map(_._2.numeratorUOM).distinct
    assert(ccys.size == 1, "No unique valuation currency")
    ccys.head
  }
}

case class UnknownPricingFixation(fraction : Double, price : Quantity)
case class UnknownPricingSpecification(
  index : IndexWithKnownPrice,
  month : Month,
  fixations : List[UnknownPricingFixation],
  declarationDay : Day,
  premium : Quantity
)
  extends TitanPricingSpec
{

  def settlementDay = month.lastDay.addBusinessDays(index.businessCalendar, 2)

  def price(env: Environment) = {
    val totalFixed = fixations.map(_.fraction).sum
    val thirdWednesday = month.firstDay.dayOnOrAfter(DayOfWeek.wednesday) + 14
    val unfixedPriceDay = if (env.marketDay >= thirdWednesday.endOfDay)
      month.lastDay.thisOrPreviousBusinessDay(index.businessCalendar)
    else
      thirdWednesday
    val unfixedFraction = 1.0 - totalFixed
    val fixedPayment = Quantity.sum(fixations.zipWithIndex.map{ case (f, i) => f.price.named("Fix_" + i) * f.fraction}).named("Fixed")
    val unfixedPayment = (index.fixingOrForwardPrice(env, unfixedPriceDay) * unfixedFraction).named("Unfixed")
    fixedPayment + unfixedPayment
  }

  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) = totalQuantity * fixations.map(_.fraction).sum
  def isComplete(marketDay : DayAndTime) = declarationDay.endOfDay >= marketDay
  def pricingType : String = "Unknown"
  def quotationPeriodStart : Option[Day] = Some(month.firstDay)
  def quotationPeriodEnd : Option[Day] = Some(month.lastDay)
  def indexName : String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = index.observationDays(month).filter(_.endOfDay() > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)
}

case class PhysicalMetalAssignment(commodityName : String, quantity : Quantity, deliveryDay : Day, pricingSpec : TitanPricingSpec) extends UTP with Tradeable {
  import PhysicalMetalAssignment._

  def isLive(dayAndTime: DayAndTime) = !pricingSpec.isComplete(dayAndTime)

  def valuationCCY = pricingSpec.valuationCCY

  def detailsForUTPNOTUSED :Map[String, Any] = Map()

  def instrumentType = PhysicalMetalAssignment
  def tradeableType = PhysicalMetalAssignment

  def persistedTradeableDetails : Map[String, Any] = Map(commodityLabel -> commodityName, deliveryDayLabel -> deliveryDay, quantityLabel -> quantity, pricingSpecLabel -> PersistAsBlob(pricingSpec))
  override def shownTradeableDetails = Map(commodityLabel -> commodityName, deliveryDayLabel -> deliveryDay, pricingSpecNameLabel -> pricingSpec.pricingType, quantityLabel -> quantity)

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(copy(quantity = Quantity(1.0, quantity.uom)) -> quantity.value))

  private def cashMtm(env : Environment) : Quantity = cashMtmWithBreakdown(env)._1
  private def cashMtmWithBreakdown(env : Environment) : (Quantity, (NamedQuantity, NamedQuantity)) = {
    val price = pricingSpec.price(env).named("F")
    val quantityInMarketUOM = quantity.inUOM(price.denominatorUOM).named("Volume")
    (- (price * quantityInMarketUOM), (price, quantityInMarketUOM))
  }

  private def discount(env : Environment) = env.discount(valuationCCY, pricingSpec.settlementDay).named("Discount")
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
        settlementDay = pricingSpec.settlementDay,
        amount = quantity,
        mtm = cashMtm_ * discount(env)
      )
    )
  }

  def price(env: Environment) = pricingSpec.price(env)

  def periodKey = (pricingSpec.quotationPeriodStart, pricingSpec.quotationPeriodEnd) match {
    case (Some(d1), Some(d2)) => Some(DateRange(d1, d2))
    case _ => None
  }

  def *(scale: Double) = this.copy(quantity = quantity * scale)

  def daysForPositionReport(marketDay: DayAndTime) = pricingSpec.daysForPositionReport(marketDay)

  def volume = quantity

  override def expiryDay() = Some(pricingSpec.settlementDay)
}

object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment]{
  val deliveryDayLabel = "deliveryDay"
  val commodityLabel = "commodity"
  val quotaIDLabel = "quotaID"
  val titanTradeIDLabel = "titanTradeID"
  val quantityLabel = "Quantity"
  val pricingSpecNameLabel = "pricingSpecName"
  val pricingSpecLabel = "pricingSpec"


  val name = "Physical Metal Assignment"
  
  def createTradeable(row: RichInstrumentResultSetRow) = {
    val commodityName = row.getString(commodityLabel)
    val quantity = row.getQuantity(quantityLabel)
    val deliveryDay = row.getDay(deliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](pricingSpecLabel)
    PhysicalMetalAssignment(commodityName, quantity, deliveryDay, pricingSpec)
  }
  def sample = PhysicalMetalAssignment("Aluminium", Quantity(100, UOM.MT), Day(2011, 10, 10), MonthAveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity.NULL))
}
