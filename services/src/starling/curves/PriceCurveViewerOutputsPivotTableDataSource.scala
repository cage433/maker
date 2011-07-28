package starling.curves

import scala.collection.SortedSet

import starling.daterange._
import starling.marketdata._
import starling.pivot._
import starling.quantity.Quantity
import collection.immutable.List
import starling.db.MarketDataReader
import starling.market.{CommodityMarket, FuturesMarket}
import starling.utils.Pattern.Extractor

import starling.utils.ImplicitConversions._


object PriceCurveFactory extends CurveType {
  def create(marketDataReader: MarketDataReader, envSpec: EnvironmentSpecification) = {
    val recordingReader = new RecordingMarketDataReader(marketDataReader)
    val outputs = new PriceCurveViewerOutputsPivotTableDataSource(envSpec.observationDay, envSpec.environmentRule, recordingReader)
    val inputs = new CurveViewerInputsPivotTableDataSource(recordingReader.recorded)

    new UnionPivotTableDataSource(outputs, inputs)
  }
}

case class UnderlyingDeliveryPeriods(observationTimeOfDay:ObservationTimeOfDay, market:CommodityMarket, periods:SortedSet[DateRange]) {
  require(market.tenor.isOneOf(Day, Month), "Only daily and monthly markets are supported")

  def dateRangesFrom(startDay: Day) = market.tenor match {
    case Day => (startDay upto periods.last.lastDay).toList
    case Month => startDay.containingMonth upto periods.last.lastMonth
  }
}

case class MarketOptionData(observationTimeOfDay:ObservationTimeOfDay, market:CommodityMarket, oilVolSurfaceData: OilVolSurfaceData) {
  def periods: SortedSet[DateRange] = SortedSet[DateRange]() ++ oilVolSurfaceData.periods.toSet
}

case class MarketFixingPeriods(market:CommodityMarket, days:SortedSet[Day])

case class CurvePrice(price:PivotQuantity, interpolated:Boolean)


class CurveViewerInputsPivotTableDataSource(inputs:Set[(TimedMarketDataKey, MarketData)]) extends UnfilteredPivotTableDataSource {

  import CurveViewerFields._

  val fieldDetailsGroups = {
    FieldDetailsGroup("Fields", market, exchange, marketTenor, commodity, period, input, timeOfDay) :: Nil
  }

  val allRows = {
    inputs.toList.flatMap {
      case (TimedMarketDataKey(observationPoint, SpotFXDataKey(ccy)), SpotFXData(rate)) => {
        List( Map(marketField -> ccy.toString,
              inputField -> PivotQuantity(rate),
              timeOfDayField -> observationPoint.timeOfDay) )
      }
      case (TimedMarketDataKey(observationPoint, PriceDataKey(market:FuturesMarket)), priceData:PriceData) => {
        priceData.prices.map { case (period, quantity) => {
          Map(marketField -> market.name,
              marketTenor.field -> market.tenor.toString,
              exchange.field -> market.exchange.name,
              commodity.field -> market.commodity.name,
              periodField -> period,
              inputField -> quantity.pq,
              timeOfDayField -> observationPoint.timeOfDay)
        } }
      }
      case (TimedMarketDataKey(observationPoint, PriceDataKey(market:CommodityMarket)), priceData:PriceData) => {
        List()
      }
    }
  }

  def unfilteredData(pfs: PivotFieldsState) = allRows
}

object CurveViewerFields {
  val showInterpolatedReportOption = "Show Interpolated"
  val hideDailyPricesOption = "Hide Daily Prices"

  val commodity = new FieldDetails("Commodity")
  val exchange = new FieldDetails("Exchange")

  val market = new FieldDetails("Market")
  val marketField = market.field

  val marketTenor = new FieldDetails("Market Tenor")

  val period = new FieldDetails("Period")
  val periodField = period.field

  val timeOfDay = new FieldDetails("Observation Time")
  val timeOfDayField = timeOfDay.field

  val input = new AveragePivotQuantityFieldDetails("Input")
  val inputField = input.field

  val price = new FieldDetails("Price") {
    override def isDataField = true
    override def value(a: Any) = {
      val set = a.asInstanceOf[Set[CurvePrice]]
      set.size match {
        case 0 => Set()
        case 1 => set.iterator.next.price
        case n => n + " values"
      }
    }
    override def formatter = AnonPricePivotFormatter
  }

  val priceField = price.field
}

class PriceCurveViewerOutputsPivotTableDataSource(observationDay: Day, rule: EnvironmentRule, marketDataReader:MarketDataReader) extends UnfilteredPivotTableDataSource {
  import CurveViewerFields._

  val fieldDetailsGroups = {
    FieldDetailsGroup("Fields", market, exchange, marketTenor, commodity, period, price, timeOfDay) :: Nil
  }

  override def reportSpecificOptions = List(showInterpolatedReportOption → List(false, true),
                                            hideDailyPricesOption → List(false, true))

  val allRows = {

    val context = rule.createEnv(observationDay, marketDataReader)

    val startDay = context.environment.marketDay.day

    val outputData = context.markets.flatMap { dataForMarket => {
        val market = dataForMarket.market

        for (period <- dataForMarket dateRangesFrom startDay) yield {
          val priceForDay = PivotQuantity.calcOrCatch(context.environment.forwardPrice(market, period))
          val interpolated = !dataForMarket.periods.contains(period)
          val price = CurvePrice(priceForDay, interpolated)
          val exchangeStr = market match {
            case f: FuturesMarket => f.exchange.name
            case _ => "None"
          }

          Map(marketField -> market.name,
              marketTenor.field -> market.tenor.toString,
              exchange.field -> exchangeStr,
              commodity.field -> market.commodity.name,
              periodField -> period,
              priceField -> price,
              timeOfDayField -> dataForMarket.observationTimeOfDay)
        }
    } }

    outputData
  }

  def unfilteredData(pfs: PivotFieldsState) = {
    val data: List[Map[Field, Object]] = if (showInterpolated(pfs)) {
      allRows
    } else {
      allRows.filterNot { row =>
        row.get(priceField).map(_.asInstanceOf[CurvePrice].interpolated).getOrElse(false)
      }
    }

    data.applyIf(hideDailyPrices(pfs), makeMonthly _)
  }

  def makeMonthly(data: List[Map[Field, Object]]): List[Map[Field, Object]] = data.collect {
    case Month(row) => row
    case ThirdWednesday(row) => row.mapValue(periodField, _.asInstanceOf[DateRange].firstMonth)
  }

  private val Month = Extractor((row: Map[Field, Object]) => row(periodField) partialMatch { case month: Month => row })
  private val ThirdWednesday = Extractor((row: Map[Field, Object]) =>
    row(periodField) partialMatch { case day: Day if day.containingMonth.thirdWednesday == day => row })

  def showInterpolated(pfs: PivotFieldsState) =
    pfs.reportSpecificChoices.getOrElse(showInterpolatedReportOption, false).asInstanceOf[Boolean]

  def hideDailyPrices(pfs: PivotFieldsState) =
    pfs.reportSpecificChoices.getOrElse(hideDailyPricesOption, false).asInstanceOf[Boolean]
}
