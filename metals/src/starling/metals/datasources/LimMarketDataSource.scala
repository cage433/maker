package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.market._
import starling.marketdata._
import starling.utils.{Pattern, Log}
import Pattern._
import starling.utils.ImplicitConversions._
import starling.lim.{LIMService, LIMConnection, LimNode}
import starling.db.{MarketDataSet, MarketDataEntry, MarketDataSource}
import starling.calendar.BusinessCalendar
import org.joda.time.LocalTime
import starling.scheduler.{ScheduledTime, SimpleScheduledTask}
import org.joda.time.{Period => JodaPeriod}
import com.lim.mimapi.RelType


abstract class LimMarketDataSource(service: LIMService, val marketDataType: MarketDataTypeName) extends MarketDataSource with Log {
  val marketDataSet = MarketDataSet.LimMetals

  protected def descriptionFor(sources: List[LimSource]) =
    marketDataType.name.pairWith(sources.flatMap(_.description)).map("%s → %s" % _)

  protected def getValuesForType(start: Day, end: Day, sources: List[LimSource]): ((Day, Day, MarketDataTypeName), List[MarketDataEntry]) =
    (start, end, marketDataType) → sources.flatMap(source => getValues(source, start, end).toList)
      //.require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)

  protected def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = service.query { connection =>
    source.marketDataEntriesFrom(connection, start, end)
      .map(_.copy(tag = Some("%s (%s)" % (source.getClass.getSimpleName, source.description.mkString(", ")))))
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .debugV(entries => "%s (%s): %s values" % (source.getClass.getSimpleName, source.description.mkString(", "), countData(entries)))
  }

  protected val notImplemented = SimpleScheduledTask(_ => println("Task Not Implemented")).update(_.disable)

  protected def limDaily(cal: BusinessCalendar, bloombergToLimTime: LocalTime): ScheduledTime =
    ScheduledTime.daily(cal, bloombergToLimTime.plus(JodaPeriod.minutes(30))) // 5 minutes for Lim import, 25 to allow for the lim team to discover & fix

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size).sum

  protected def earliestDayToImport(day: Day) = day - 7 // day.startOfFinancialYear
}

case class Prices[Relation](relation: Relation, priceByLevel: Map[Level, Double], observationDay: Day) {
  def atTimeOfDay(observationTimeOfDay: ObservationTimeOfDay) = observationDay.atTimeOfDay(observationTimeOfDay)
  def priceFor(level: Level): Double = priceByLevel(level)
}

abstract class LimSource(val levels: List[Level]) {
  type Relation
  protected def relationsFrom(connection: LIMConnection): List[(Relation, String)] = Nil
  protected def marketDataEntriesFrom(fixings: List[Prices[Relation]]): Iterable[MarketDataEntry] = Nil

  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day): List[MarketDataEntry] = {
    val relations = relationsFrom(connection)

    val prices = relations.flatMap { case (relation, childRelation) => {
      val prices = connection.getPrices(childRelation, levels, start, end).flipNesting

      prices.toList.flatMapO {
        case (observationDay, pricesForLevel) =>
          if (pricesForLevel.isEmpty) None else Some(Prices(relation, pricesForLevel, observationDay))
      }
    } }

    marketDataEntriesFrom(prices).toList
  }

  def description: List[String]
  protected def levelDescription = "(" + levels.map(_.name).mkString(", ") + ")"
}

abstract class HierarchicalLimSource(val parentNodes: List[LimNode], levels: List[Level],
  relationTypes: Set[RelType] = Set(RelType.CATEGORY)) extends LimSource(levels) {

  def description = parentNodes.map(node => node.name + " " + levelDescription)

  override def relationsFrom(connection: LIMConnection) =
    connection.getAllRelChildren(parentNodes, relationTypes).flatMap(safeRelationFrom)

  def relationExtractor: Extractor[String, Option[Relation]]

  private def safeRelationFrom(childRelation: String): Option[(Relation, String)] = try {
    relationExtractor.unapply(childRelation).flatOpt.optPair(childRelation)
  } catch { case exception => { log.debug("Malformed LIM relation: " + childRelation, exception); None } }
}