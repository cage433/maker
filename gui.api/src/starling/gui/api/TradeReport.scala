package starling.gui.api

import collection.immutable.{TreeSet, SortedSet}

import starling.calendar.BusinessCalendar
import starling.daterange._
import starling.tradestore.TradePredicate
import starling.utils.{StarlingEnum, ImplicitConversions, SColumn}
import starling.varcalculator.NAhead
import scalaz.Scalaz._

import ImplicitConversions._
import starling.quantity.{NamedQuantity, Quantity}
import starling.pivot.StackTrace

class TradeReport

case class MarketDataSelection(pricingGroup:Option[PricingGroup] = None, excel:Option[String]= None) {
  def isNull = !pricingGroup.isDefined && !excel.isDefined
  def text = {
    (pricingGroup match { case Some(pg) => pg.toString; case None => "" }) + " " +
      (excel match { case Some(name) => name; case None => "" })
  }
  def noExcel = copy(excel=None)

  def pricingGroupMatches(predicate : PricingGroup => Boolean) = pricingGroup.exists(predicate)
}
object MarketDataSelection {
  val Null = MarketDataSelection(None, None)
}
trait MarketDataVersion {
  def label:String
}

case class SnapshotType(name:String)

object SnapshotType extends StarlingEnum(classOf[SnapshotType], (st:SnapshotType) => st.name) {
  val Manual = new SnapshotType("Manual")
  val MarketData = new SnapshotType("Market Data")
  val Valuation = new SnapshotType("Valuation")
}
case class SpecificMarketDataVersion(version:Int) extends MarketDataVersion {
  def label = "v" + version
}
case class SnapshotMarketDataVersion(snapshotLabel:SnapshotIDLabel) extends MarketDataVersion {
  def label = "s" + snapshotLabel.id
}
object SnapshotMarketDataVersion {
  def apply(optLabel: Option[SnapshotIDLabel]): Option[SnapshotMarketDataVersion] = optLabel.map(SnapshotMarketDataVersion(_))
}
trait MarketDataPageIdentifier {
  def filteredMarketData:Boolean
  def selection:MarketDataSelection = marketDataIdentifier.selection
  def marketDataIdentifier:MarketDataIdentifier
  def isCurrent:Boolean = marketDataIdentifier.isCurrent
}
case class StandardMarketDataPageIdentifier(marketDataIdentifier:MarketDataIdentifier) extends MarketDataPageIdentifier {
  def filteredMarketData = false
}
case class ReportMarketDataPageIdentifier(reportParameters:ReportParameters) extends MarketDataPageIdentifier {
  def filteredMarketData = true
  def marketDataIdentifier = reportParameters.curveIdentifier.marketDataIdentifier
}

case class MarketDataIdentifier(selection: MarketDataSelection, marketDataVersion: MarketDataVersion) {
  def isNull = selection.isNull
  def pricingGroupMatches(predicate : PricingGroup => Boolean) = selection.pricingGroupMatches(predicate)
  def copyVersion(version: Int) = copy(marketDataVersion = SpecificMarketDataVersion(version))
  def isCurrent:Boolean = marketDataVersion match {
    case x:SpecificMarketDataVersion => true
    case _ => false
  }
}

object MarketDataIdentifier {
  def apply(selection: MarketDataSelection, version: Int) =
    new MarketDataIdentifier(selection, new SpecificMarketDataVersion(version))
}

/**
 * explanation is Either a String (of an exception) indicating an error, or the named quantity explanation
 */
case class TradeValuation(explanation: Either[StackTrace, NamedQuantity])
case class TradeValuationAndDetails(tradeValuation:TradeValuation, tradeRow:List[Any], fieldDetailsGroups:List[FieldDetailsGroupLabel], columns:List[SColumn])

case class PricingGroupDefinition(pricingGroup:PricingGroup, validMarketDataTypes:List[MarketDataTypeLabel])
case class PricingGroup(name:String) {
  override def toString = name
}

object PricingGroup extends StarlingEnum(classOf[PricingGroup], (pg: PricingGroup) => pg.name) {
  val Metals = PricingGroup("Metals")
  val System = PricingGroup("System")
  val LimOnly = PricingGroup("LIM Only")
  val Crude = PricingGroup("Crude")
  val LondonDerivativesOptions = PricingGroup("London Derivatives Options")
  val LondonDerivatives = PricingGroup("London Derivatives")
  val BarryEckstein = PricingGroup("Barry Eckstein")
  val GasolineRoW = PricingGroup("Gasoline RoW")
  val GasOil = PricingGroup("Gas Oil")
  val Naphtha = PricingGroup("Naphtha")
}

case class FieldDetailsGroupLabel(groupName:String, childNames:List[String])

case class SnapshotIDLabel(id: Int, timestamp : Timestamp, marketDataSelection:MarketDataSelection, snapshotType:SnapshotType, version: Int) extends Ordered[SnapshotIDLabel] {
  def compare(rhs: SnapshotIDLabel) = id - rhs.id
  def shortString = timestamp.toStringSecondsShort + " (s" + id + ", " + snapshotType.name + ")"
  def snapshotDay = timestamp.day
}

trait DeskInfo

case class EAIDeskInfo(book: Int) extends DeskInfo

sealed case class Desk(name: String, pricingGroups: List[PricingGroup], deskInfo: Option[DeskInfo] = None) {
  override def toString = name
}

case class EnabledDesks(desks: Set[Desk])

object Desk extends StarlingEnum(classOf[Desk], (d: Desk) => d.name, ignoreCase = true) {
  import PricingGroup._

  val LondonDerivativesOptions = Desk("London Derivatives Options", List(PricingGroup.LondonDerivativesOptions, System, LimOnly), Some(EAIDeskInfo(173)))
  val LondonDerivatives = Desk("London Derivatives", List(PricingGroup.LondonDerivatives, System, LimOnly), Some(EAIDeskInfo(43)))
  val GasolinePhysicalBargesAndARABlending = Desk("Gasoline Physical Barges & ARA blending", List(PricingGroup.GasolineRoW, LimOnly), Some(EAIDeskInfo(117)))
  val GasoilSpec = Desk("Gasoil Spec", List(PricingGroup.GasOil, LimOnly), Some(EAIDeskInfo(22)))
  val GasolineSpec = Desk("Gasoline Spec Global", List(PricingGroup.GasolineRoW, LimOnly), Some(EAIDeskInfo(149)))
  val CrudeSpecNorthSea = Desk("Crude Spec North Sea", List(Crude, LimOnly), Some(EAIDeskInfo(197)))
  val HoustonDerivatives = Desk("Houston Derivatives", List(BarryEckstein, LimOnly), Some(EAIDeskInfo(190)))
  val NaphthaSpec = Desk("Naphtha Spec", List(PricingGroup.Naphtha, LimOnly), Some(EAIDeskInfo(20)))
  val CED = Desk("Central Execution Desk", List(PricingGroup.LondonDerivatives, System, LimOnly), Some(EAIDeskInfo(150)))

  val Titan = Desk("Titan", List(Metals))

  val pricingGroups = values.flatMap(_.pricingGroups).distinct

  def eaiDesks = values.flatMap {
    case d@Desk(_, _, Some(info: EAIDeskInfo)) => Some(d)
    case _ => None
  }

  def eaiDeskFromID(bookID: Int) = eaiDesks.find {
    case d@Desk(_, _, Some(info: EAIDeskInfo)) => info.book == bookID
    case _ => false
  }

  def oilDesks = Set(GasolinePhysicalBargesAndARABlending, GasoilSpec, GasolineSpec, CrudeSpecNorthSea, HoustonDerivatives, NaphthaSpec)
}

class TradeEvent
case class DeskTradeEvent(name:String) extends TradeEvent
case object ExcelTradeEvent extends TradeEvent

case class TradeSystemLabel(name:String, shortCode:String) {override def toString = name}

case class TradeExpiryDay(exp: Day) {
  override def toString = exp.toString
}

object TradeExpiryDay {
  val ThisFinancialYear = TradeExpiryDay(Day.today.startOfFinancialYear)
  val types:List[TradeExpiryDay] = {
    val td = Day.today;
    ThisFinancialYear :: (td - 10 upto td).filter(_.isWeekday).map(TradeExpiryDay(_)).toList
  }
}

case class IntradayGroups(subgroups:List[String])

object TradeTimestamp{
  val magicNoBookClosesDay = Day(1980, 1, 1)
  val magicLatestTimestampDay = Day(2100, 1, 1)

  def makeMagicLatestTimestamp(timestamp : Timestamp) = TradeTimestamp(
      timestamp,
      TradeTimestamp.magicLatestTimestampDay,
      1,
      None
    )
}
case class TradeTimestamp(timestamp:Timestamp, closeDay: Day, closeNumber: Int, error: Option[String]) extends Ordered[TradeTimestamp] {
  import TradeTimestamp._

  def compare(that: TradeTimestamp) = {
    timestamp.compare(that.timestamp) match {
      case 0 => closeDay - that.closeDay
      case other => other
    }
  }

  def asString = closeDay match {
    case `magicNoBookClosesDay` => ""
    case `magicLatestTimestampDay` => "Current"
    case _ => closeDay + " v" + closeNumber
  }
  override def toString = asString
}

case class TradeSelection(desk:Option[Desk], tradePredicate:TradePredicate, intradaySubgroup:Option[IntradayGroups]) {
  def withDeskTimestamp(deskTimestamp: TradeTimestamp) = {
    assert(intradaySubgroup.isEmpty, "Can't create timestamped trade selection when using intradaySubgroup")
    new TradeSelectionWithTimestamp(desk.map((_, deskTimestamp)), tradePredicate, None)
  }
  def withTimestamp(deskTimestamp: TradeTimestamp, timestamp: (IntradayGroups) => Timestamp) = {
    val groupsWithTimestamp = intradaySubgroup.map(g => (g, timestamp(g)))
    new TradeSelectionWithTimestamp(desk.map((_, deskTimestamp)), tradePredicate, groupsWithTimestamp)
  }
}

case class TradeSelectionWithTimestamp(deskAndTimestamp:Option[(Desk, TradeTimestamp)], tradePredicate:TradePredicate,
                                   intradaySubgroupAndTimestamp:Option[(IntradayGroups, Timestamp)]) {
  def asTradeSelection = new TradeSelection(deskAndTimestamp.map(_._1), tradePredicate, intradaySubgroupAndTimestamp.map(_._1))

  def desk = deskAndTimestamp.map(_._1)

  def copyWithNewIntradaySubgroupTimestamp(timestamp: Timestamp) = {
    copy(intradaySubgroupAndTimestamp = Some((intradaySubgroupAndTimestamp.get._1, timestamp)))
  }

  def isTitanTrades = desk == Some(Desk.Titan)
}

case class TradePageParameters(deskAndTimestamp:Option[(Desk, TradeTimestamp)],
        intradaySubgroupAndTimestamp:Option[(IntradayGroups, Timestamp)],
        expiry:TradeExpiryDay)

case class PnlFromParameters(tradeTimestampFrom: Option[TradeTimestamp], curveIdentifierFrom:CurveIdentifierLabel)

case class ReportParameters(tradeSelectionWithTimestamp:TradeSelectionWithTimestamp, curveIdentifier:CurveIdentifierLabel,
                            reportOptions:ReportOptions, expiryDay:Day,
         pnlParameters:Option[PnlFromParameters] = None, runReports:Boolean = true) {
  def text = {
    shortText + " (" + tradeSelectionWithTimestamp + " " + curveIdentifier + ")"
  }
  def shortText = reportOptions.options.map(_.name).mkString("Report for ", ", ", "") + (reportOptions.slide1 match {
    case None => ""
    case Some(x) => {
      reportOptions.slide2 match {
        case None => " (1D Slide)"
        case Some(z) => " (2D Slide)"
      }
    }
  })

  def copyWithIntradayTimestamp(timestamp: Timestamp) = {
    copy(tradeSelectionWithTimestamp = tradeSelectionWithTimestamp.copyWithNewIntradaySubgroupTimestamp(timestamp))
  }
  def desk = tradeSelectionWithTimestamp.deskAndTimestamp.map(_._1)
}

case class EnvironmentModifierLabel(name:String)
object EnvironmentModifierLabel {
  val zeroInterestRates = EnvironmentModifierLabel("ZeroInterestRates")
  val zeroVols = EnvironmentModifierLabel("ZeroVols")

  implicit object ordering extends Ordering[EnvironmentModifierLabel]{
    def compare(lhs : EnvironmentModifierLabel, rhs : EnvironmentModifierLabel) : Int = lhs.name.compare(rhs.name)
  }
}

case class CurveIdentifierLabel(
        marketDataIdentifier:MarketDataIdentifier,
        environmentRule:EnvironmentRuleLabel,
        observationDayAndTime:DayAndTime, //trades with a trade day after this value are ignored
        forwardValuationDayAndTime:DayAndTime, //typically the same as tradesUpToDay but can be moved forward
        thetaToDayAndTime:DayAndTime,
        envModifiers:SortedSet[EnvironmentModifierLabel]) {

  def copyVersion(version: Int) = copy(marketDataIdentifier = marketDataIdentifier.copyVersion(version))
}

object CurveIdentifierLabel{
  def defaultLabelFromSingleDay(marketDataIdentifier : MarketDataIdentifier, calendar : BusinessCalendar, zeroInterestRates : Boolean = false) = {
    val day = Day.today.previousWeekday // don't use a calendar as we don't know which one
    val timeOfDayToUse = if (day >= Day.today) TimeOfDay.StartOfDay else TimeOfDay.EndOfDay
    val envModifiers = if (zeroInterestRates)
      TreeSet[EnvironmentModifierLabel](EnvironmentModifierLabel.zeroInterestRates)
    else
      TreeSet.empty[EnvironmentModifierLabel](EnvironmentModifierLabel.ordering)
    CurveIdentifierLabel(
      marketDataIdentifier,
      EnvironmentRuleLabel.COB,
      day.endOfDay,
      day.atTimeOfDay(timeOfDayToUse),
      day.nextBusinessDay(calendar).endOfDay,
      envModifiers
    )
  }
}

/**
 * For most reports it is sufficient to join trades to pivot reports using
 * the integer id of the utp. However for Jon's spread delta report it was necessary
 * to combine rows based on the trade's associated strategy
 */
case class UTPIdentifier(id : Int, attributes : Map[String, String] = Map()){
  def getAttribute(key : String) = attributes.get(key)
}

case class ReportErrors(errors:List[ReportError])
case class ReportError(instrumentID:Int , instrumentText:String, stackTrace:StackTrace)

case class PriceHistory(prices:List[PriceEntry])
case class PriceEntry(observationDay:Day, market:String, ahead:NAhead, price:Quantity, forwardStatePrice : Quantity)
case class VolEntry(market:String, ahead:NAhead, vol:Double)
case class CorrelationEntry(marketA:String, aheadA:NAhead, marketB:String, aheadB:NAhead, correlation:Double)
case class HistoricVols(vols:List[VolEntry])
case class HistoricCorrelations(correlations:List[CorrelationEntry])

case class TradeIDLabel(id:String, tradeSystem:TradeSystemLabel) {
  override def toString = id
  val name = tradeSystem.shortCode + id
}

case class TradeImportResult(updatedTradeCount:Int, newTradeCount:Int, errorTradeCount:Int, uniqueErrorsCount:Int, time:Timestamp) {
  def +(other:TradeImportResult) = copy(
    updatedTradeCount=updatedTradeCount+other.updatedTradeCount,
    newTradeCount=newTradeCount+other.newTradeCount,
    errorTradeCount=errorTradeCount+other.errorTradeCount,
    uniqueErrorsCount=uniqueErrorsCount+other.uniqueErrorsCount,
    // This is a bit of a hack to ensure we have the latest version.
    time = if (time > other.time) time else other.time
  )
  val details = {
    (time.toStringMinutes,"Added " + newTradeCount + ", Updated " + updatedTradeCount + (if (errorTradeCount > 0) {
      ", Errors " + errorTradeCount + ", Distinct Errors " + uniqueErrorsCount
    } else {
      ""
    }))
  }
  val isEmpty = {
    updatedTradeCount == 0 && newTradeCount == 0
  }
}

case class ReferenceDataLabel(name:String)
object MarketDataTypeLabel {
  val Default = MarketDataTypeLabel("Price")
}
case class MarketDataTypeLabel(name:String) {
  override def toString = name
}
