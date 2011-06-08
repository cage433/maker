package starling.gui.api

import collection.immutable.{TreeSet, SortedSet}
import java.io.Serializable

import starling.calendar.BusinessCalendar
import starling.daterange._
import starling.quantity.Quantity
import starling.rmi.StarlingServer
import starling.tradestore.TradePredicate
import starling.utils.{Named, StarlingEnum, ImplicitConversions, STable}
import starling.varcalculator.NAhead

import ImplicitConversions._


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
trait MarketDataVersion {
  def label:String
}
case class SpecificMarketDataVersion(version:Int) extends MarketDataVersion {
  def label = "v" + version
}
case class SnapshotMarketDataVersion(snapshotLabel:SnapshotIDLabel) extends MarketDataVersion {
  def label = "s" + snapshotLabel.id
}
trait MarketDataPageIdentifier {
  def filteredMarketData:Boolean
  def selection:MarketDataSelection = marketDataIdentifier.selection
  def marketDataIdentifier:MarketDataIdentifier
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
}

object MarketDataIdentifier {
  def apply(selection: MarketDataSelection, version: Int) =
    new MarketDataIdentifier(selection, new SpecificMarketDataVersion(version))
}

class TradeValuation(val valuationParameters:STable) extends Serializable

case class PricingGroup(name:String) extends Named {
  override def toString = name
}

object PricingGroup extends StarlingEnum(classOf[PricingGroup]) {
  val Metals = PricingGroup("Metals")
  val System = PricingGroup("System")
  val LimOnly = PricingGroup("LIM Only")
  val Crude = PricingGroup("Crude")
  val LondonDerivativesOptions = PricingGroup("London Derivatives Options")
  val LondonDerivatives = PricingGroup("London Derivatives")
  val BarryEckstein = PricingGroup("Barry Eckstein")
  val GasolineRoW = PricingGroup("Gasoline RoW")
}

case class FieldDetailsGroupLabel(groupName:String, childNames:List[String])

case class SnapshotIDLabel(observationDay: Day, id: Int, timestamp : Timestamp, version: Int) extends Ordered[SnapshotIDLabel] {
  def compare(rhs: SnapshotIDLabel) = {
    if (observationDay == rhs.observationDay) {
      id - rhs.id
    } else {
      observationDay.compareTo(rhs.observationDay)
    }
  }
  def shortString = observationDay + " (s" + id + ")"
  def identifier() = observationDay+"-"+id + "-" + timestamp
}


sealed case class Desk(name: String) extends Named {
  import Desk._
  import PricingGroup._

  override def toString = name

  def pricingGroups = this match {
    case Desk.LondonDerivativesOptions => List(PricingGroup.LondonDerivativesOptions, System, LimOnly)
    case Desk.LondonDerivatives        => List(PricingGroup.LondonDerivatives, System, LimOnly)
    case Desk.GasolineSpec             => List(PricingGroup.GasolineRoW, LimOnly)
    case CrudeSpecNorthSea             => List(Crude, LimOnly)
    case HoustonDerivatives            => List(BarryEckstein, LimOnly)
    case Refined                       => List(System, Metals)
  }
}

object Desk extends StarlingEnum(classOf[Desk]) {
  val LondonDerivativesOptions = Desk("London Derivatives Options")
  val LondonDerivatives = Desk("London Derivatives")
  val GasolineSpec = Desk("Gasoline Spec Global")
  val CrudeSpecNorthSea = Desk("Crude Spec North Sea")
  val HoustonDerivatives = Desk("Houston Derivatives")
  val Refined = Desk("Refined")

  val pricingGroups = values.flatMap(_.pricingGroups).distinct

  private def label(tradeSystem:String, shortCode: String) = TradeSystemLabel(tradeSystem, shortCode)
}

class TradeEvent
case class DeskTradeEvent(name:String) extends TradeEvent
case object ExcelTradeEvent extends TradeEvent

case class TradeSystemLabel(name:String, shortCode:String) {override def toString = name}

object TradeSystemLabel {
  val Intraday = TradeSystemLabel("Intraday", "int")
}

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

case class TradeTimestamp(timestamp:Timestamp, closeDay: Day, closeNumber: Int, error: Option[String]) {
  def noCloseDay = closeDay.year == 1980

  def asString = if(noCloseDay) {
    "" // this is so that we can not have anything in combo box choosers
  } else {
    closeDay + " v" + closeNumber
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
}

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
        tradesUpToDay:Day, //trades with a trade day after this value are ignored
        valuationDayAndTime:DayAndTime, //typically the same as tradesUpToDay but can be moved forward
        thetaDayAndTime:DayAndTime,
        envModifiers:SortedSet[EnvironmentModifierLabel]) {

  def copyVersion(version: Int) = copy(marketDataIdentifier = marketDataIdentifier.copyVersion(version))

  private def contains(daysForPricingGroup: Map[PricingGroup, Set[Day]]) : Boolean = {
    marketDataIdentifier.pricingGroupMatches(pricingGroup => daysForPricingGroup.contains(pricingGroup, tradesUpToDay))
  }
}

object CurveIdentifierLabel{
  def defaultLabelFromSingleDay(marketDataIdentifier : MarketDataIdentifier, day : Day, calendar : BusinessCalendar) = {
    val timeOfDayToUse = if (day >= Day.today) TimeOfDay.StartOfDay else TimeOfDay.EndOfDay
    CurveIdentifierLabel(
      marketDataIdentifier,
      EnvironmentRuleLabel.COB,
      day,
      day.atTimeOfDay(timeOfDayToUse),
      day.nextBusinessDay(calendar).endOfDay(),
      TreeSet.empty[EnvironmentModifierLabel](EnvironmentModifierLabel.ordering)
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
case class ReportError(instrumentID:Int , instrumentText:String, message:String)

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
case class MarketDataTypeLabel(name:String) {
  override def toString = name
}
