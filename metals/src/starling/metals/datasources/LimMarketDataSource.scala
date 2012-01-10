package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import starling.marketdata._
import starling.utils.Log
import starling.utils.ImplicitConversions._
import starling.lim.{LIMService, LIMConnection}
import starling.db.{MarketDataSet, MarketDataEntry, MarketDataSource}
import starling.calendar.BusinessCalendar
import org.joda.time.LocalTime
import starling.scheduler.{ScheduledTime, SimpleScheduledTask}
import org.joda.time.{Period => JodaPeriod}


abstract class LimMarketDataSource(service: LIMService, val marketDataType: MarketDataTypeName) extends MarketDataSource with Log {
  val marketDataSet = MarketDataSet.LimMetals

  protected def descriptionFor(sources: List[LimSource]) =
    marketDataType.name.pairWith(sources.flatMap(_.description)).map("%s → %s" % _)

  protected def getValuesForType(start: Day, end: Day, sources: List[LimSource]): ((Day, Day, MarketDataTypeName), List[MarketDataEntry]) =
    (start, end, marketDataType) → sources.flatMap(source => getValues(source, start, end).toList)
      //.require(containsDistinctTimedKeys, "concatenated sources: %s, produced duplicate MarketDataKeys: " % sources)

  protected def getValues(source: LimSource, start: Day, end: Day): List[MarketDataEntry] = service.query { connection =>
    source.marketDataEntriesFrom(connection, start, end).toList
      .map(_.copy(tag = Some("%s (%s)" % (source.name, source.description.mkString(", ")))))
      .require(containsDistinctTimedKeys, "source: %s produced duplicate MarketDataKeys: " % source)
      .debugV(entries => "%s (%s): %s values" % (source.name, source.description.mkString(", "), countData(entries)))
  }

  protected val notImplemented = SimpleScheduledTask(_ => println("Task Not Implemented")).update(_.disable)

  protected def limDaily(cal: BusinessCalendar, bloombergToLimTime: LocalTime): ScheduledTime =
    ScheduledTime.daily(cal, bloombergToLimTime.plus(JodaPeriod.minutes(30))) // 5 minutes for Lim import, 25 to allow for the lim team to discover & fix

  private def countData(entries: List[MarketDataEntry]) = entries.map(_.data.size).sum

  protected def earliestDayToImport(day: Day) = day - 7 // day.startOfFinancialYear
}

trait LimSource {
  def name: String = getClass.getSimpleName
  def description: List[String]
  def marketDataEntriesFrom(connection: LIMConnection, start: Day, end: Day): Iterable[MarketDataEntry]
}