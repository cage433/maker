package starling.metals.datasources

import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.utils.ImplicitConversions._
import scalaz.Scalaz._
import collection.immutable.Map
import starling.daterange.TimeZone
import org.joda.time.LocalTime


object DBBloombergImports {
  def importsFrom(eai: DB) = BloombergImports(getTimeZones(eai) |> { timeZones =>
    eai.queryWithResult(select("*") from ("tblQuotesBloomberg")) { rs =>
      timeZones.get(rs.getInt("ExpectedAvailableTimeZoneID")).map { timeZone => BloombergImport(
        rs.getInt("QuoteID"),
        rs.getStringOption("BloombergSymbol"),
        rs.getStringOption("LimSymbol"),
        rs.getStringOption("LimFolder"),
        rs.getStringOption("LimColumn"),
        rs.getStringOption("LimDescription"),
        rs.getInt("ExportToLim") == 1,
        new LocalTime(rs.getInt("ExpectedAvailableHour"), rs.getInt("ExpectedAvailableMinute")),
        timeZone
      ) }
    }.flatten
  })

  private def getTimeZones(eai: DB) = eai.lookupTable("tblTimezones", "ID", "WindowsID").mapKeys(_.toInt) innerJoin Map(
    "UTC"                      → TimeZone.UTC,
    "GMT Standard Time"        → TimeZone.GMT,
    "China Standard Time"      → TimeZone.Shanghai,
    "US Eastern Standard Time" → TimeZone.USEastern,
    "Central Standard Time"    → TimeZone.USCentral,
    "Tokyo Standard Time"      → TimeZone.Tokyo
  )
}