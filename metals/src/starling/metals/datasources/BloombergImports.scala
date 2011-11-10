package starling.metals.datasources

import collection.immutable.List

import starling.daterange._
import scalaz.Scalaz._
import org.joda.time.{DateTime, LocalTime}
import starling.utils.ImplicitConversions._
import starling.market.{LimSymbol, FuturesMarket}


case class BloombergImports(imports: List[BloombergImport]) {
  def matches(folder: String, market: FuturesMarket): Boolean = market.limSymbol.fold(matches(folder, market, _), false) ||
    matchesLimSymbol(folder, "TRAF.%s.%s" % (market.exchange.name, market.commodity.name.toUpperCase))

  def matches(folder: String, market: FuturesMarket, symbol: LimSymbol): Boolean =
    matchesLimSymbol(folder, "TRAF.%s.%s" % (market.exchange.name, symbol.name)) || containsLimSymbol(folder, symbol.name)

  def containsLimSymbol(folder: String, symbol: String) = forFolder(folder).exists(_.limSymbol == Some(symbol))
  def matchesLimSymbol(folder: String, pattern: String) = forFolder(folder).exists(_.matchesLimSymbol(pattern))

  private def forFolder(folder: String) = imports.filter(i => i.exportToLim && i.limFolder == Some(folder))
}

case class BloombergImport(quoteId: Int, symbol: Option[String], limSymbol: Option[String], limFolder: Option[String],
  limColumn: Option[String], limDescription: Option[String], exportToLim: Boolean, expectedTime: LocalTime, timeZone: TimeZone) {

  def matchesLimSymbol(pattern: String): Boolean = limSymbol.fold(_.contains(pattern), false)
  def expectedTime(other: TimeZone): DateTime = dateTime.withZone(other)

  private lazy val dateTime = Day.today |> { today => new DateTime(today.year, today.month, today.dayNumber,
    expectedTime.getHourOfDay, expectedTime.getMinuteOfHour, 0, 0, timeZone)
  }
}