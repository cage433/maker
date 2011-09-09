package starling.marketdata

import starling.quantity.Quantity
import starling.market.Commodity
import starling.pivot._

case class Hub(name : String)
case class Location(name : String)
case class Grade(name : String)


/**
 * benchmark data for (grade, hub) to quantity
 */
case class GradeHubBenchmarkData(hubData : List[((Grade,Hub),Quantity)]) extends MarketData {
  def size = hubData.size
}

/**
 * Benchmark hub market data key represents a list of grade, hub market data rows keyed per commodity
 */
case class GradeHubBenchmarkMarketDataKey(commodity : Commodity) extends MarketDataKey {

  import GradeHubBenchmarkDataType._
  type marketDataType = GradeHubBenchmarkData
  type marketDataDBType = GradeHubBenchmarkData
  def dataType = GradeHubBenchmarkDataType
  def subTypeKey = commodity.toString
  def fieldValues = Map(commodityField.field -> commodity.name)
  def rows(marketData : GradeHubBenchmarkData) : List[Map[Field, Any]] = {
    marketData.hubData.map {
      case ((grade, hub), price) => Map(
        commodityField.field -> commodity.name,
        gradeField.field -> grade.name,
        hubField.field -> hub.name,
        benchmarkPriceField.field -> price
      )
    }.toList
  }
}

object GradeHubBenchmarkDataType extends MarketDataType {
  type dataType = GradeHubBenchmarkData

  val commodityField = FieldDetails("Commodity")
  val hubField = FieldDetails("Hub")
  val gradeField = FieldDetails("Grade")
  val benchmarkPriceField = FieldDetails("Benchmark Price")

  def marketDataKeyFelds = Set(commodityField.field)
  override def keyFields = Set(commodityField, hubField, gradeField).map(_.field)
  override def valueFields = Set(benchmarkPriceField.field)
  val fields = List(commodityField, hubField, gradeField, benchmarkPriceField)
  val initialPivotState = PivotFieldsState(
    filters=List((commodityField.field,SomeSelection(Set()))),
    dataFields=List(benchmarkPriceField.field),
    rowFields=List(hubField.field, gradeField.field),
    columnFields=List()
  )

  def createKey(values : Map[Field, Any]) = GradeHubBenchmarkMarketDataKey(Commodity.fromName(values(commodityField.field).asInstanceOf[String]))

  def createValue(values : List[Map[Field, Any]]) = {
    val data = values.map {
      rowMap =>
        val grade = Grade(rowMap(gradeField.field).asInstanceOf[String])
        val hub = Hub(rowMap(hubField.field).asInstanceOf[String])
        val benchmarkPrice = rowMap(benchmarkPriceField.field).asInstanceOf[Quantity]
        ((grade, hub), benchmarkPrice)
    }
    GradeHubBenchmarkData(data)
  }
}

object BenchmarkLocationMarketDataType extends MarketDataType {
  type dataType = BenchmarkLocationData

  val commodityField = FieldDetails("Commodity")
  val locationField = FieldDetails("Location")
  val gradeField = FieldDetails("Grade")
  val benchmarkPriceField = FieldDetails("Benchmark Price")

  def marketDataKeyFelds = Set(commodityField.field)
  override def keyFields = Set(commodityField, locationField, gradeField).map(_.field)
  override def valueFields = Set(benchmarkPriceField.field)
  val fields = List(commodityField, locationField, gradeField, benchmarkPriceField)
  val initialPivotState = PivotFieldsState(
    filters = List((commodityField.field,SomeSelection(Set()))),
    dataFields = List(benchmarkPriceField.field),
    rowFields = List(locationField.field, gradeField.field),
    columnFields = List()
  )

  def createKey(values : Map[Field, Any]) = BenchmarkLocationMarketDataKey(Commodity.fromName(values(commodityField.field).asInstanceOf[String]))

  def createValue(values : List[Map[Field, Any]]) = {
    var marketDataMap = Map[Location, Quantity]()
    values.foreach{
      rowMap =>
        val location = Location(rowMap(locationField.field).asInstanceOf[String])
        val benchmarkPrice = rowMap(benchmarkPriceField.field).asInstanceOf[Quantity]
        marketDataMap += location -> benchmarkPrice
    }
    BenchmarkLocationData(marketDataMap)
  }
}

/**
 * Benchmark hub market data key represents a list of grade, location market data rows keyed per commodity
 */
case class BenchmarkLocationMarketDataKey(commodity : Commodity) extends MarketDataKey {

  import BenchmarkLocationMarketDataType._
  type marketDataType = BenchmarkLocationData
  type marketDataDBType = BenchmarkLocationData
  def dataType = BenchmarkLocationMarketDataType
  def subTypeKey = commodity.toString
  def fieldValues = Map(commodityField.field -> commodity.name)
  def rows(marketData : BenchmarkLocationData) : List[Map[Field, Any]] = {
    marketData.locationData.map {
      case (location, price) => Map(
        commodityField.field -> commodity.name,
        locationField.field -> location.name,
        benchmarkPriceField.field -> price
      )
    }.toList
  }
}

/**
 * benchmark data for location to quantity
 */
case class BenchmarkLocationData(locationData : Map[Location, Quantity]) extends MarketData {
  def size = locationData.size
}
