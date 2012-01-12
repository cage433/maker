package starling.services

import collection.immutable.List

import starling.calendar.BusinessCalendars
import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.ReferenceDataLabel
import starling.market._
import starling.market.formula.FormulaIndex
import starling.pivot._
import starling.pivot.model.PivotTableModel
import starling.utils.ImplicitConversions._
import scala.collection.JavaConversions._

case class ReferenceData(name:String, pivot:PivotTableDataSource)

/**
 * Represents reference data (calendars, markets, ...) as pivots where possible
 */
class ReferenceDataService() {

  def add(referenceData:ReferenceData) {
    referenceDatas.put(referenceData.name, referenceData.pivot)
  }

  private val referenceDatas = new java.util.concurrent.ConcurrentHashMap[String,PivotTableDataSource]()

  //Built in reference data - only static dependencies
  add(ReferenceData("Futures Markets", ReferenceDataService.futuresMarketPivot()))
  add(ReferenceData("Futures Front Period Indexes", ReferenceDataService.futuresFronPeriodIndexes()))
  add(ReferenceData("Formula Indexes", ReferenceDataService.formulaIndexes()))
  add(ReferenceData("Published Indexes", ReferenceDataService.publishedIndexes()))


  def referenceDataTables():List[ReferenceDataLabel] = {
    referenceDatas.map{ t => ReferenceDataLabel(t._1) }.toList.sortWith(_.name.toLowerCase < _.name.toLowerCase)
  }

  def referencePivot(table: ReferenceDataLabel, pivotFieldParams: PivotFieldParams) = {
    val dataSource = referenceDatas.find(_._1==table.name).get._2
    PivotTableModel.createPivotData(dataSource, pivotFieldParams)
  }

}

object ReferenceDataService {

  def futuresMarketPivot() = {
    new UnfilteredPivotTableDataSource() {
      val name       = FieldDetails("Name")
      val lotSize    = FieldDetails("Lot Size")
      val volumeUnit = FieldDetails("Volume Unit")
      val currency   = FieldDetails("Currency")
      val eaiQuoteID = FieldDetails("EAI Quote ID")
      val tenor      = FieldDetails("Tenor")
      val calendar   = FieldDetails("Calendar")
      val exchange   = FieldDetails("Exchange")
      val commodity  = FieldDetails("Commodity")
      val volatilityID = FieldDetails("Volatility ID")
      val hasOptions = FieldDetails("Has Options")
      val limSymbol  = FieldDetails("LIM Symbol")
      val trinityCommodity = FieldDetails("Trinity Commodity")
      val trinityUpload = FieldDetails("Trinity Upload")

      private val otherFields = List(lotSize, volumeUnit, currency, eaiQuoteID, tenor, calendar, exchange, commodity, volatilityID, hasOptions, limSymbol, trinityCommodity, trinityUpload)
      def fieldDetailsGroups = List(FieldDetailsGroup("Futures Market",
        name :: otherFields
      ))

      override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields)))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Market.futuresMarketsView.map { market =>
          fields(
            name       → market.name,
            lotSize    → market.lotSize.getOrElse("").toString,
            volumeUnit → market.uom.toString,
            currency   → market.currency,
            eaiQuoteID → market.eaiQuoteID.toString,
            tenor      → market.tenor.toString,
            calendar   → market.businessCalendar.name,
            exchange   → market.exchange.name,
            commodity  → market.commodity.toString,
            volatilityID → market.volatilityID,
            hasOptions → (if(market.hasOptions) "Options" else "No options"),
            limSymbol  → market.limSymbol.getOrElse("").toString,
            trinityCommodity → TrinityMarket.marketToTrinityCode.getOrElse(market, "")
          )
        }
        data
      }
    }
  }

  def formulaIndexes() = {
    new UnfilteredPivotTableDataSource() {
      val name       = FieldDetails("Name")
      val volumeUnit = FieldDetails("Volume Unit")
      val currency   = FieldDetails("Currency")
      val formula    = FieldDetails("Formula")
      val conversionToMT    = FieldDetails("Conversion to MT")
      val available    = FieldDetails("Available")

      private val otherFields = List(volumeUnit, currency, formula, conversionToMT, available)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields)))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Index.formulaIndexesView.flatMap { index => index partialMatch {
          case fi:FormulaIndex => fields(
            name → fi.name,
            volumeUnit → fi.uom,
            currency → fi.ccy,
            formula → fi.formulaString,
            conversionToMT → fi.conversion.map(_.toString),
            available → fi.isValid
          )
        } }
        data
      }
    }
  }

  def publishedIndexes() = {
    new UnfilteredPivotTableDataSource() {
      val name       = FieldDetails("Name")
      val market     = FieldDetails("Market")
      val eaiQuoteID = FieldDetails("EAI Quote ID")
      val limSymbol  = FieldDetails("Lim Symbol")
      val level      = FieldDetails("Level")
      val volumeUnit = FieldDetails("Volume Unit")
      val currency   = FieldDetails("Currency")
      val tenor      = FieldDetails("Tenor")
      val commodity  = FieldDetails("Commodity")
      val rounding  = FieldDetails("Rounding")

      private val otherFields = List(market, eaiQuoteID, level, volumeUnit, currency, tenor, commodity, rounding)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields)))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Index.allView.flatMap { index => index partialMatch {
          case pi:PublishedIndex => fields(
            name       → pi.name,
            market     → pi.market.name,
            eaiQuoteID → pi.eaiQuoteID.toString,
            limSymbol  → pi.market.limSymbol.getOrElse("").toString,
            level      → pi.level.name,
            volumeUnit → pi.market.currency,
            currency   → pi.market.currency,
            tenor      → pi.market.tenor.toString,
            commodity  → pi.market.commodity.toString,
            rounding  → pi.precision.toString
          )
        } }
        data
      }
    }
  }

  def futuresFronPeriodIndexes() = {
    new UnfilteredPivotTableDataSource() {
      val name       = FieldDetails("Name")
      val market     = FieldDetails("Market")
      val eaiQuoteID = FieldDetails("EAI Quote ID")
      val limSymbol  = FieldDetails("Lim Symbol")
      val level      = FieldDetails("Level")
      val volumeUnit = FieldDetails("Volume Unit")
      val currency   = FieldDetails("Currency")
      val tenor      = FieldDetails("Tenor")
      val commodity  = FieldDetails("Commodity")
      val rounding  = FieldDetails("Rounding")

      private val otherFields = List(market, eaiQuoteID, level, volumeUnit, currency, tenor, commodity, rounding)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields)))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Index.allView.flatMap { index => index partialMatch {
          case fi:FuturesFrontPeriodIndex => fields(
            name       → fi.name,
            market     → fi.market.name,
            eaiQuoteID → fi.eaiQuoteID.toString,
            limSymbol  → fi.market.limSymbol.getOrElse("").toString,
            level      → fi.level.name,
            volumeUnit → fi.market.currency,
            currency   → fi.market.currency,
            tenor      → fi.market.tenor.toString,
            commodity  → fi.market.commodity.toString,
            rounding  → fi.precision.toString
          )
        } }
        data
      }
    }
  }

  def calendars(businessCalendars:BusinessCalendars) = {
    new UnfilteredPivotTableDataSource() {
      val calendar  = FieldDetails("Calendar")
      val isHoliday = FieldDetails("Is Holiday")
      val dayField  = FieldDetails("Day")
      val year      = FieldDetails("Year")

      def fieldDetailsGroups = List(FieldDetailsGroup("Calendar", calendar, dayField, year, isHoliday))

      private val initialYears = (Day.today.year until Day.today.year + 3).toSet.asInstanceOf[Set[Any]]
      override val initialState = DefaultPivotState(PivotFieldsState(dataFields=List(isHoliday.field), rowFields=List(dayField.field), columnFields=List(calendar.field), filters=List( (year.field, new SomeSelection(initialYears)) )))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = businessCalendars.calendars.flatMap { c =>
          c.holidays.map { day => fields(
            calendar  → c.name,
            dayField  → day,
            year      → day.year,
            isHoliday → "Yes"
          ) }
        }
        data
      }
    }
  }

  def pricingGroups(marketDataStore:MarketDataStore) = {
    new UnfilteredPivotTableDataSource() {
      val pricingGroup = FieldDetails("Pricing Group")
      val source       = FieldDetails("Source")

      def fieldDetailsGroups = List(FieldDetailsGroup("Pricing Groups", pricingGroup, source))

      override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(pricingGroup, source)))

      val data = marketDataStore.pricingGroupMarketDataSets.flatMap { case (pg, marketDataSets) =>
          marketDataSets.zipWithIndex.map { case (marketDataSet, index) =>
            fields(
              pricingGroup → pg.toString,
              source       → marketDataSet.name.withIndex(index)
            )
          }
        }.toList
      def unfilteredData(pfs : PivotFieldsState) = data
    }
  }
}