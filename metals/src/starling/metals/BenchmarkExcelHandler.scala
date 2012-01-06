package starling.metals

import starling.loopyxl.ExcelMethod
import org.boris.xlloop.reflect.XLFunction
import org.boris.xlloop.reflect.XLFunction._
import starling.market.Commodity
import starling.marketdata.NeptuneCountryCode._
import starling.marketdata.GradeCode._
import starling.quantity.Quantity._
import starling.marketdata.CountryBenchmarkData._
import starling.marketdata.CountryBenchmarkMarketDataKey._
import starling.utils.ImplicitConversions._
import starling.daterange.{ObservationPoint, Tenor}
import starling.db.{MarketDataStore, MarketDataSet}
import starling.quantity.{Quantity, UOM}
import starling.marketdata._
import starling.pivot.{PivotFormatter, PivotQuantity, PivotQuantityPivotParser}
import collection.immutable.Map
import collection.mutable.ArraySeq

class BenchmarkExcelHandler(marketDataStore:MarketDataStore, neptuneReferenceData:ReferenceDataLookup) {

  @ExcelMethod
  @XLFunction(category = "Starling", name = "uploadBenchmarks",
    args=Array("data", "label"),
    argHelp = Array(
      "A range with the headers Commodity, Grade, Country, Effective Month, Price, Unit",
      "an optional name for the data, defaults to Official:Metal"
    )
  )
  def uploadBenchmarks(headerAndData: Array[Array[Object]], label:String) = {

    implicit def objectToHasBlank(o:Object) = new Object {
      def isBlank = {
        o match {
          case x if x == null => true
          case s:String => s.trim.isEmpty
          case _ => false
        }
      }
      def nonBlank = !isBlank
    }

    val header: Map[String, Int] = headerAndData.head.zipWithIndex.filter(_._1.nonBlank).toMap.mapKeys(_.toString.trim.toLowerCase) withDefault
      ( (k) => throw new Exception("The header must have a column for " + k.capitalize))

    implicit def arrayWithTextMethod(a:Array[Object]) = new Object {
      def text(name:String) = {
        val index = header(name)
        val t = a(index).toString.trim
        if (t.isEmpty) {
          throw new Exception("There is no value for " + name)
        } else {
          t
        }
      }
    }
    val data = headerAndData.tail
    val lastNonNullValues = data.head
    val parsedRows = data.toList.flatMap{ row => {
      if (row.forall(v => v == null || v.toString.trim.isEmpty)) {
        Nil
      } else {
        row.zipWithIndex.foreach { case (value, index) => if (value.nonBlank) lastNonNullValues(index) = value}
        val populatedRow: Array[Object] = row.mapWithIndex{ case(value,index) =>
          if (value.isBlank) lastNonNullValues(index) else value}.toArray
        val commodity = Commodity.fromName(populatedRow.text("commodity"))
        val country = neptuneReferenceData.countryCodeFor(populatedRow.text("country"))
        val grade = neptuneReferenceData.gradeCodeFor(populatedRow.text("grade"))
        val tenor = Tenor.parse(populatedRow.text("effective month"))
        val benchmark = {
          val bm = PivotQuantityPivotParser.parse( row.text("benchmark price"),
            PivotFormatter.DefaultExtraFormatInfo)._1.asInstanceOf[PivotQuantity].quantityValue.get
          if (bm.isScalar) {
            val uomText = row.text("uom")
            Quantity(bm.value, UOM.fromString(uomText))
          } else {
            bm
          }
        }
        (commodity, ((country, grade), (tenor, benchmark))) :: Nil
      }
    } }
    val blankEntries = Commodity.all.map(c => CountryBenchmarkMarketDataKey(c) -> CountryBenchmarkData(Map()))
    val entries = blankEntries ++ parsedRows.groupInto(_._1, _._2).map { case (commodity, rows) => {
      val v = rows.groupInto(_._1, _._2).mapValues(_.groupInto(_._1, _._2).mapValues(_.head))
      val marketData = CountryBenchmarkData(v)
      CountryBenchmarkMarketDataKey(commodity) -> marketData
    }}.toMap
    val marketDataSet = if (label.isBlank) MarketDataSet.ManualMetals else MarketDataSet.excel(label)
    val result = marketDataStore.saveAll(
      marketDataSet,
      ObservationPoint.RealTime,
      entries.map{ case(k,v) => (k.asInstanceOf[MarketDataKey], v.asInstanceOf[MarketData])}.toMap)
    "OK:" + result.maxVersion
  }

}