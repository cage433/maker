package starling.curves.readers

import collection.immutable.List

import starling.{LIMConnection, LimNode, LIMServer}
import starling.daterange._
import starling.db.{MarketDataEntry, MarketDataSource}
import starling.market._
import starling.marketdata._
import starling.scheduler.SimpleScheduledTask
import starling.utils.{Pattern, Log}
import Pattern._
import starling.utils.ImplicitConversions._


abstract class LimMarketDataSource(limServer: LIMServer) extends MarketDataSource with Log {
  protected def getValuesForType(m: MarketDataTypeName, start: Day, end: Day, sources: (MarketDataTypeName, List[LimSource])) =
    (start, end, sources.head) → sources.tail.flatMap(source => getValues(source, start, end).toList)
      .require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)

  protected def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = limServer.query { connection =>
    val relations = source.relationsFrom(connection)

    val prices = relations.flatMap { case (fixingRelation, childRelation) => {
      val prices = source.levels.toMapWithValues(level => connection.getPrices(childRelation, level, start, end))

      prices.flipNesting.toList.flatMapO { case (observationDay, pricesForLevel) =>
        if (pricesForLevel.isEmpty) None else Some(Prices(fixingRelation, pricesForLevel, observationDay))
      }
    } }

    source.marketDataEntriesFrom(prices).toList
      .map(_.copy(tag = Some("%s (%s)" % (source.getClass.getSimpleName, source.description.mkString(", ")))))
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .debugV(entries => "%s (%s): %s values" % (source.getClass.getSimpleName, source.description.mkString(", "), countData(entries)))
  }

  protected val notImplemented = SimpleScheduledTask(_ => println("Task Not Implemented"))

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size).sum
}

case class Prices[Relation](relation: Relation, priceByLevel: Map[Level, Double], observationDay: Day) {
  def atTimeOfDay(observationTimeOfDay: ObservationTimeOfDay) = observationDay.atTimeOfDay(observationTimeOfDay)
  def priceFor(level: Level) = priceByLevel(level)
}

abstract class LimSource(val levels: List[Level]) {
  type Relation
  def relationsFrom(connection: LIMConnection): List[(Relation, String)]
  def marketDataEntriesFrom(fixings: List[Prices[Relation]]): Iterable[MarketDataEntry]
  def description: List[String]
  protected def levelDescription = "(" + levels.map(_.name).mkString(", ") + ")"
  protected def exchangeLookup(exchange: String) = if (exchange == "SHFE") "SFS" else exchange
}

abstract class HierarchicalLimSource(val parentNodes: List[LimNode], levels: List[Level]) extends LimSource(levels) with Log {
  def description = parentNodes.map(node => node.name + " " + levelDescription)
  def relationsFrom(connection: LIMConnection) = connection.getAllRelChildren(parentNodes : _*).flatMap(safeRelationFrom)
  def relationExtractor: Extractor[String, Option[Relation]]

  private def safeRelationFrom(childRelation: String): Option[(Relation, String)] = try {
    relationExtractor.unapply(childRelation).flatOpt.optPair(childRelation)
  } catch { case exception => { log.debug("Malformed LIM relation: " + childRelation, exception); None } }
}