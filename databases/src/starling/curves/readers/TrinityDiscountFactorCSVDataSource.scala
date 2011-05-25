package starling.curves.readers

import starling.db.{MarketDataEntry, MarketDataSource}
import io.Source
import starling.quantity.UOM
import starling.daterange.{ObservationPoint, ObservationTimeOfDay, Day}
import java.lang.String
import org.joda.time.format.DateTimeFormat
import starling.utils.ImplicitConversions._
import starling.marketdata._
import java.io.File

class TrinityDiscountFactorCSVDataSource extends MarketDataSource {
  val ignoredCurrencies = List("BRL", "COP", "HKD", "INR", "PEN")
  private val path = "TrinityCurvePrice.csv"
  private val trinityInstrumentType: String = "ShortDepoSwap"
  private val timeOfDay = ObservationTimeOfDay.Default

  def read(observationDay: Day) = if (new File(path).exists) {
    Map((observationDay, observationDay, ForwardRateDataType) -> readLines(observationDay, Source.fromFile(path).getLines.toList))
  } else {
    Map()
  }

  def readLines(observationDay: Day, lines: List[String]) = {
    val observationPoint = ObservationPoint(observationDay, timeOfDay)

    lines.zipWithIndex.flatMap(parseLine(_)).groupBy(_.currency)
      .map(g => marketDataEntry(observationPoint, g._1, g._2))
      .toList
      .update(validateHaveResults)
  }

  def parseLine(indexedLine: (String, Int)): Option[Row] = {
    val (line, index) = indexedLine
    val entries = line.split(',').toList

    if (entries.exists(_.isEmpty)) {
      throw new Exception("Line %d is malformed: %s " % (index, line))
    }

    val (instrumentType :: currency :: day :: _ :: discountFactor :: Nil) = entries

    if (instrumentType == trinityInstrumentType && !ignoredCurrencies.contains(currency)) {
      Some(Row.fromStrings(currency, day, discountFactor))
    } else {
      None
    }
  }

  private def marketDataEntry(observationPoint: ObservationPoint, currency: UOM, rows: List[Row]): MarketDataEntry = {
    new MarketDataEntry(observationPoint, ForwardRateDataKey(currency), ForwardRateData(rows.map(_.forwardRateDateEntry)))
  }

  private def validateHaveResults(result: scala.List[MarketDataEntry]) {
    if (result.isEmpty) throw new Exception("There is no data")
  }

  case class Row(currency: UOM, day: Day, discountFactor: Double) {
    val forwardRateDateEntry = ForwardRateDataEntry(day, "Discount", trinityInstrumentType, discountFactor)
  }

  object Row {
    val dateFormat = DateTimeFormat.forPattern("dd'-'MMM'-'yyyy")

    def fromStrings(currency: String, day: String, discountFactor: String): Row = {
      Row(UOM.fromString(currency), Day.parseWithFormat(day, dateFormat), discountFactor.toDouble)
    }
  }
}