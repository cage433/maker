package starling.services

import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import collection.immutable.Map
import starling.daterange.{TimeZone, Day}
import org.joda.time.{LocalTime, DateTime}


object BloombergImport {
  def importsFrom(eai: DB): List[BloombergImport] = getTimeZones(eai) |> { timeZones =>
    eai.queryWithResult(select("*") from ("tblQuotesBloomberg")) { rs =>
      timeZones.get(rs.getInt("ExpectedAvailableTimeZoneID")).map { timeZone => BloombergImport(
        rs.getInt("QuoteID"),
        rs.getStringOption("BloombergSymbol"),
        rs.getStringOption("LimSymbol"),
        rs.getStringOption("LimFolder"),
        rs.getStringOption("LimColumn"),
        rs.getStringOption("LimDescription"),
        new LocalTime(rs.getInt("ExpectedAvailableHour"), rs.getInt("ExpectedAvailableMinute")),
        timeZone
      ) }
    }.flatten
  }

  private def getTimeZones(eai: DB) = eai.lookupTable("tblTimezones", "ID", "WindowsID").mapKeys(_.toInt) >>> Map(
    "UTC"                      → TimeZone.UTC,
    "GMT Standard Time"        → TimeZone.GMT,
    "China Standard Time"      → TimeZone.Shanghai,
    "US Eastern Standard Time" → TimeZone.USEastern,
    "Central Standard Time"    → TimeZone.USCentral,
    "Tokyo Standard Time"      → TimeZone.Tokyo
  )
}

case class BloombergImport(quoteId: Int, symbol: Option[String], limSymbol: Option[String], limFolder: Option[String],
  limColumn: Option[String], limDescription: Option[String], expectedTime: LocalTime, timeZone: TimeZone) {

  def expectedTime(other: TimeZone): DateTime = dateTime.withZone(other)

  private lazy val dateTime = Day.today |> { today => new DateTime(today.year, today.month, today.dayNumber,
    expectedTime.getHourOfDay, expectedTime.getMinuteOfHour, 0, 0, timeZone)
  }
}