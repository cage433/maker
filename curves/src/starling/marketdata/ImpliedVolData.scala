package starling.marketdata


import collection.SortedMap
import starling.quantity.{UOM, Percentage}
import starling.daterange.{DateRange, Day}
import starling.market.{InterestRateMarket, Market, FXMarket, HasImpliedVol}
import collection.immutable.TreeMap
import starling.pivot.{Field, PivotFieldsState, SomeSelection, FieldDetails}

/**
 * These should no longer be needed. Originally held implied vols gathered from Trinity reval tables, however
 * we now read vols from volpages/voldata.
 * Some Conversion scripts refer to these classes.
 * TODO [28 Jun 2010] remove after next release - AMc
 */

object ImpliedVolDataType extends MarketDataType {
  type dataType = ImpliedVolData
  val marketField: FieldDetails = FieldDetails("Market")
  val periodField: FieldDetails = FieldDetails("Period")
  val strikeField: FieldDetails = FieldDetails("Strike")
  val exerciseDayField: FieldDetails = FieldDetails("Exercise Day")
  val volatilityField: FieldDetails = FieldDetails("Volatility")

  val initialPivotState = PivotFieldsState(
    filters=List((marketField.field,SomeSelection(Set()))),
    dataFields=List(volatilityField.field),
    rowFields=List(periodField.field),
    columnFields=List(strikeField.field)
  )

  val fields = List(
    marketField,
    periodField,
    strikeField,
    exerciseDayField,
    volatilityField)

  def keyFields = throw new Exception("Implement if we need to do Metals VAR")
  def valueFields = throw new Exception("Implement if we need to do Metals VAR")
  def createKey(values: Map[Field, Any]) = throw new Exception("Implement if we need to do Metals VAR")
  def createValue(values: List[Map[Field, Any]]) = throw new Exception("Implement if we need to do Metals VAR")
}

case class ImpliedVolEntryKey(period: DateRange, strike: Double, exerciseDay: Day)

object ImpliedVolEntryKey extends Ordering[ImpliedVolEntryKey] {
  def compare(o1: ImpliedVolEntryKey, o2: ImpliedVolEntryKey) = o1.period.compare(o2.period) match {
    case 0 => o1.strike.compare(o2.strike) match {
      case 0 => o1.exerciseDay.compare(o2.exerciseDay)
      case r => r
    }
    case r => r
  }
}

case class ImpliedVolDataKey(market : HasImpliedVol) extends MarketDataKey {
  type marketDataType = ImpliedVolData
  type marketDataDBType = ImpliedVolData
  def dataType = ImpliedVolDataType
  def subTypeKey = market.name

  override def rows(data : ImpliedVolData) = data.surface.map((tuple2) => {
        val (key, vol) = tuple2
        Map(
          ImpliedVolDataType.marketField.field -> market.name,
          ImpliedVolDataType.periodField.field -> key.period,
          ImpliedVolDataType.strikeField.field -> key.strike,
          ImpliedVolDataType.exerciseDayField.field -> key.exerciseDay,
          ImpliedVolDataType.volatilityField.field -> vol)

      })
  def fieldValues = Map(ImpliedVolDataType.marketField.field -> market.name)
}

case class ImpliedVolData(
  surface : SortedMap[ImpliedVolEntryKey, Percentage]
) extends MarketData