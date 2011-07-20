package starling.instrument.physical

import starling.richdb.RichInstrumentResultSetRow
import starling.curves.Environment
import starling.daterange._
import starling.instrument._
import starling.quantity.{UOM, Quantity}
import starling.utils.sql.PersistAsBlob
import starling.market.{LmeSingleIndices, SingleIndex}

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
      premium + env.forwardFXRate(premiumCurrency, priceCurrency, settlementDay) * price
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
  def index : SingleIndex
  protected def observedDays(marketDay : DayAndTime) = observationDays.filter(_.endOfDay <= marketDay)
  def isComplete(marketDay : DayAndTime) = observedDays(marketDay).size == observationDays.size
  def quotationPeriodStart : Option[Day] = Some(observationDays.head)
  def quotationPeriodEnd : Option[Day] = Some(observationDays.last)
  def indexName : String = index.name

  def daysForPositionReport(marketDay: DayAndTime) = observationDays.filter(_.endOfDay() > marketDay)

  def valuationCCY = premiumCCY.getOrElse(index.currency)
}

case class MonthAveragePricingSpec(index : SingleIndex, month : Month, premium : Quantity) extends AveragePricingSpec{
  val observationDays = index.observationDays(month)
  // Just a guess
  def settlementDay = month.lastDay.addBusinessDays(index.businessCalendar, 2)
  def price(env: Environment) = addPremiumConvertingIfNecessary(env, env.averagePrice(index, month), premium)
  def dummyTransferPricingSpec = copy(premium = Quantity.NULL)
  def fixedQuantity(marketDay : DayAndTime, totalQuantity : Quantity) : Quantity = totalQuantity * observedDays(marketDay).size / observationDays.size
  def pricingType : String = "Month Average"
}

case class PartialAveragePricingSpec(index : SingleIndex, dayFractions : Map[Day, Double], premium : Quantity) extends AveragePricingSpec {

  val observationDays = dayFractions.keySet.toList.sortWith(_<_)
  def settlementDay = dayFractions.keys.toList.sortWith(_>_).head.addBusinessDays(index.businessCalendar, 2)

  def price(env: Environment) = {
    addPremiumConvertingIfNecessary(
      env,
      dayFractions.map {
        case (day, frac) => env.fixingOrForwardPrice(index, day) * frac
      }.sum,
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
  def price(env: Environment) = specs.map{case (weight, spec) => spec.price(env) * weight}.sum

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
      pricesByFraction.map{
        case (qty, prc) => prc * qty
      }.sum / totalFraction
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
  index : SingleIndex,
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
    val fixedPayment = fixations.map{f => f.price * f.fraction}.sum
    val unfixedPayment = env.fixingOrForwardPrice(index, unfixedPriceDay) * unfixedFraction
    unfixedPayment + fixedPayment
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

case class PhysicalMetalAssignment(commodityName : String, quantity : Quantity, deliveryDay : Day, pricingSpec : TitanPricingSpec, quotaID : String, titanTradeID : String) extends UTP with Tradeable {
  import PhysicalMetalAssignment._

  def isLive(dayAndTime: DayAndTime) = !pricingSpec.isComplete(dayAndTime)

  def valuationCCY = pricingSpec.valuationCCY

  def details :Map[String, Any] = Map(commodityLabel -> commodityName, deliveryDayLabel -> deliveryDay, pricingSpecNameLabel -> pricingSpec.pricingType, quotaIDLabel -> quotaID, titanTradeIDLabel -> titanTradeID)

  def instrumentType = PhysicalMetalAssignment
  def tradeableType = PhysicalMetalAssignment

  def tradeableDetails : Map[String, Any] = (details - pricingSpecNameLabel) ++ Map(quantityLabel -> quantity, pricingSpecLabel -> PersistAsBlob(pricingSpec))

  def asUtpPortfolio(tradeDay:Day) = UTP_Portfolio(Map(copy(quantity = Quantity(1.0, quantity.uom)) -> quantity.value))

  def assets(env: Environment) = {
    Assets(
      // Physical
      Asset(
        known = true,
        assetType = commodityName,
        settlementDay = deliveryDay,
        amount = quantity,
        mtm = Quantity.NULL
      ),
      // Cash
      Asset(
        known = pricingSpec.isComplete(env.marketDay),
        assetType = commodityName,
        settlementDay = pricingSpec.settlementDay,
        amount = quantity,
        mtm = quantity * pricingSpec.price(env) * -1
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
}

object PhysicalMetalAssignment extends InstrumentType[PhysicalMetalAssignment] with TradeableType[PhysicalMetalAssignment]{
  val deliveryDayLabel = "Delivery"
  val commodityLabel = "Commodity"
  val quotaIDLabel = "Quota ID"
  val titanTradeIDLabel = "Titan Trade ID"
  val quantityLabel = "Quantity"
  val pricingSpecNameLabel = "Pricing Spec Name"
  val pricingSpecLabel = "Pricing Spec"


  val name = "Physical Metal Assignment"
  
  def createTradeable(row: RichInstrumentResultSetRow) = {
    val commodityName = row.getString(commodityLabel)
    val quantity = row.getQuantity(quantityLabel)
    val deliveryDay = row.getDay(deliveryDayLabel)
    val pricingSpec = row.getObject[TitanPricingSpec](pricingSpecLabel)
    val quotaID = row.getString(quotaIDLabel)
    val titanTradeID = row.getString(titanTradeIDLabel)
    PhysicalMetalAssignment(commodityName, quantity, deliveryDay, pricingSpec, quotaID, titanTradeID)
  }
  def sample = PhysicalMetalAssignment("Aluminium", Quantity(100, UOM.MT), Day(2011, 10, 10), MonthAveragePricingSpec(LmeSingleIndices.alCashBid, Month(2011, 10), Quantity.NULL), "12345", "54321")
}
