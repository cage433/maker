package starling.services

import collection.immutable.List

import starling.calendar.BusinessCalendars
import starling.daterange.Day
import starling.db.MarketDataStore
import starling.eai.EAIStrategyDB
import starling.gui.api.ReferenceDataLabel
import starling.market._
import starling.market.formula.FormulaIndex
import starling.pivot._
import starling.pivot.model.PivotTableModel
import starling.services.trinity.TrinityUploadCodeMapper
import starling.utils.ImplicitConversions._

/**
 * Represents reference data (calendars, markets, ...) as pivots where possible
 */
class ReferenceData(businessCalendars: BusinessCalendars, marketDataStore: MarketDataStore, strategyDB: EAIStrategyDB,
                    scheduler: Scheduler, trinityUploadMapper: TrinityUploadCodeMapper) {

  val referenceDatas = List(
    "Futures Markets"   → futuresMarketPivot(trinityUploadMapper),
    "Forward Markets"   → forwardMarketPivot(),
    "Formula Indexes"   → formulaIndexes(),
    "Published Indexes" → publishedIndexes(),
    "Pricing Groups"    → pricingGroups(),
    "Calendars"         → calendars(businessCalendars),
    "Schedules"         → schedules(scheduler)
//    "Strategies"        → strategies(strategyDB))
    )

  def referenceDataTables():List[ReferenceDataLabel] = referenceDatas.map{ t => ReferenceDataLabel(t._1) }

  def referencePivot(table: ReferenceDataLabel, pivotFieldParams: PivotFieldParams) = {
    val dataSource = referenceDatas.find(_._1==table.name).get._2
    PivotTableModel.createPivotData(dataSource, pivotFieldParams)
  }

  def futuresMarketPivot(trinityUploadMapper: TrinityUploadCodeMapper) = {
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
      val hasOptions = FieldDetails("Has Options")
      val limSymbol  = FieldDetails("LIM Symbol")
      val trinityCommodity = FieldDetails("Trinity Commodity")
      val trinityUpload = FieldDetails("Trinity Upload")

      private val otherFields = List(lotSize, volumeUnit, currency, eaiQuoteID, tenor, calendar, exchange, commodity, hasOptions, limSymbol, trinityCommodity, trinityUpload)
      def fieldDetailsGroups = List(FieldDetailsGroup("Futures Market",
        name :: otherFields
      ))

      override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Market.futuresMarkets.map { market =>
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
            hasOptions → (if(market.hasOptions) "Options" else "No options"),
            limSymbol  → market.limSymbol.getOrElse("").toString,
            trinityCommodity → Market.marketToTrinityCode.getOrElse(market, ""),
            trinityUpload → trinityUploadMapper.uploadCodeOption(market).getOrElse("")
          )
        }
        data
      }
    }
  }

  def forwardMarketPivot() = {
    new UnfilteredPivotTableDataSource() {
      val name       = FieldDetails("Name")
      val lotSize    = FieldDetails("Lot Size")
      val volumeUnit = FieldDetails("Volume Unit")
      val currency   = FieldDetails("Currency")
      val eaiQuoteID = FieldDetails("EAI Quote ID")
      val tenor      = FieldDetails("Tenor")
      val calendar   = FieldDetails("Calendar")
      val commodity  = FieldDetails("Commodity")

      private val otherFields = List(lotSize, volumeUnit, currency, eaiQuoteID, tenor, calendar, commodity)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Market.forwardMarkets.map { market =>
          fields(
            name       → market.name,
            lotSize    → market.lotSize.getOrElse("").toString,
            volumeUnit → market.uom.toString,
            currency   → market.currency,
            eaiQuoteID → market.eaiQuoteID.toString,
            tenor      → market.tenor.toString,
            calendar   → market.businessCalendar.name,
            commodity  → market.commodity.toString
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

      private val otherFields = List(volumeUnit, currency, formula, conversionToMT)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = FormulaIndexList.formulaIndexes.filter(_.isValid).flatMap { index => index partialMatch {
          case fi:FormulaIndex => fields(
            name → fi.name,
            volumeUnit → fi.uom,
            currency → fi.ccy,
            formula → fi.formulaString,
            conversionToMT → fi.conversion.map(_.toString)
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

      private val otherFields = List(market, eaiQuoteID, level, volumeUnit, currency, tenor, commodity)
      def fieldDetailsGroups = List(FieldDetailsGroup("Forward Market",
        name :: otherFields
      ))

      override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(otherFields))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = Index.namedIndexes.flatMap { index => index partialMatch {
          case pi:PublishedIndex => fields(
            name       → pi.name,
            market     → pi.market.name,
            eaiQuoteID → pi.eaiQuoteID.toString,
            limSymbol  → pi.market.limSymbol.getOrElse("").toString,
            level      → pi.level.name,
            volumeUnit → pi.market.currency,
            currency   → pi.market.currency,
            tenor      → pi.market.tenor.toString,
            commodity  → pi.market.commodity.toString
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
      override val initialState = PivotFieldsState(dataFields=List(isHoliday.field), rowFields=List(dayField.field), columnFields=List(calendar.field), filters=List( (year.field, new SomeSelection(initialYears)) ))

      def unfilteredData(pfs : PivotFieldsState) = {
        val data = businessCalendars.calendars.flatMap { c =>
          c.days.map { day => fields(
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

  def pricingGroups() = {
    new UnfilteredPivotTableDataSource() {
      val pricingGroup = FieldDetails("Pricing Group")
      val source       = FieldDetails("Source")

      def fieldDetailsGroups = List(FieldDetailsGroup("Pricing Groups", pricingGroup, source))

      override val initialState = PivotFieldsState(rowFields = fields(pricingGroup, source))

      val data = marketDataStore.pricingGroupsDefinitions.flatMap { case (pg, marketDataSets) =>
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

  def schedules(scheduler: Scheduler) = new UnfilteredPivotTableDataSource() {
    val group@List(name, timing, period, calendar, from, to) = fieldDetails("Task", "Scheduled Time", "Period", "Calendar", "From", "To")
    val fieldDetailsGroups = List(FieldDetailsGroup("Schedule", group))
    override val initialState = PivotFieldsState(rowFields = fields(name), dataFields = fields(group.tail))
    def unfilteredData(pfs: PivotFieldsState) = scheduler.tasks.map(task => fields(name → task.name, timing → task.time.prettyTime,
      period → task.time.description, calendar → task.cal.name, from → task.attribute("DataSource"), to → task.attribute("DataSink")))
  }

//  def strategies(strategyDB:EAIStrategyDB) = {
//    new UnfilteredPivotTableDataSource() {
//      val strategyFilter = new TreeFieldDetails(treeID => strategyDB.tree.calculateNodeAndAllChildren(treeID).map(_.id), "Strategy Filter")
//      val strategy = new TreeFieldDetails(treeID => strategyDB.tree.calculateNodeAndAllChildren(treeID).map(_.id), "Strategy")
//      val label = FieldDetails("Label")
//
//      def fieldDetailsGroups = List(FieldDetailsGroup("Strategy",
//        strategyFilter :: strategy :: label :: Nil
//      ))
//
//      override val initialState = new PivotFieldsState()
//        dataFields=List(label.field),
//        rowFields=List(strategy.field),
//        filters=List( (strategyFilter.field, AllSelection) )
//      )
//
//      override def treeFields = Map(strategy.field -> strategyDB.tree, strategyFilter.field -> strategyDB.tree)
//
//      def unfilteredData(pfs : PivotFieldsState) = {
//        val data = strategyDB.tree.pathToNode.filter { case (path,node) => {
//            path.startsWith("Spec/London/London Derivatives") || path.startsWith("Spec/London/Derivatives")
//          }
//        }.map { case(path, node) =>
//          val m=Map(
//            strategyFilter.field -> node,
//            strategy.field -> node,
//            label.field -> path
//          )
//          println("Map " + m)
//          m
//        }.toList
//        PivotResult(data, Map())
//      }
//    }
//  }

}