package starling.marketdata

import starling.pivot._
import pivotparsers.{SpecifiedValuesParser, DayPivotParser}
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._
import starling.daterange.Day
import starling.quantity.{UOM, Quantity}
import starling.market.{FuturesMarket, NeptuneCommodity, Commodity}

object BenchmarkPricePivotParser extends PivotParser {
  def parse(text: String, extraFormatInfo: ExtraFormatInfo) = {
    val (pq, v) = PivotQuantityPivotParser.typedParse(text, extraFormatInfo)

    val benchmark = pq.quantityValue.get

    require(benchmark.uom.numeratorUOM.isCurrency && !benchmark.uom.denominatorUOM.isCurrency,
      benchmark + " does not have a valid unit of measure")

    (pq, v)
  }
}






/** benchmark data for (grade, area) to quantity */


/** Benchmark area market data key represents a list of grade, area market data rows keyed per commodity */




