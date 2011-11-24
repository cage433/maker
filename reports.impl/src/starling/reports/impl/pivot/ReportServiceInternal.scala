package starling.reports.impl.pivot

import greeks.GreeksPivotReport
import java.lang.String
import starling.pivot.model._
import starling.daterange._
import starling.instrument._
import starling.utils._
import starling.utils.ImplicitConversions._
import starling.curves._
import starling.db._
import starling.pivot._
import collection.immutable.{Set}
import controller.PivotTable
import starling.curves.SlideType._
import starling.gui.api._
import starling.services.trade.TradeDiff
import starling.utils.cache.CacheFactory
import starling.tradestore._
import starling.instrument.{Trade}
import starling.rmi.PivotData
import collection.mutable.ListBuffer
import starling.market._
import starling.marketdata._
import starling.quantity._
import starling.reports.impl.{MarketDataStoreReportContext, AbstractReportContext}

case class CurveIdentifier(
        marketDataIdentifier:MarketDataIdentifier,
        environmentRule:EnvironmentRule,
        tradesUpToDay:Day,
        valuationDayAndTime:DayAndTime, //Typically the same as tradesUpToDay but can be moved forward
        thetaDayAndTime:DayAndTime,
        zeroInterestRates:Boolean, zeroVols:Boolean) {
  override def toString = marketDataIdentifier + " " + tradesUpToDay
}

class CurveIdentifierFactory(environmentRules: EnvironmentRules) {
  def unLabel(label:CurveIdentifierLabel):CurveIdentifier = CurveIdentifier(
    label.marketDataIdentifier,
    environmentRules.forLabel(label.environmentRule),
    label.tradesUpToDay,
    label.valuationDayAndTime,
    label.thetaDayAndTime,
    label.envModifiers.contains(EnvironmentModifierLabel.zeroInterestRates),
    label.envModifiers.contains(EnvironmentModifierLabel.zeroVols)
  )
}


/**
 * Creates a report context from a CurveIdentifier
 */
class ReportContextBuilder(marketDataStore:MarketDataStore) {

  def contextFromCurveIdentifier(curveIdentifier: CurveIdentifier, environmentSliders: EnvironmentSlider*) : AbstractReportContext = {

    if (curveIdentifier.marketDataIdentifier.isNull) {
      new AbstractReportContext(curveIdentifier, environmentSliders.toList) {
        def atomicEnvironment(day: Day) = {
          curveIdentifier.environmentRule.createNullAtomicEnvironment(day)
        }
        def observationDays(from: Day, to: Day) = SimpleDateRange(from, to).days.toList.map(ObservationDay)
        def recorded : Set[(ObservationPoint, MarketDataKey, MarketData)] = Set()
      }
    } else {
      new MarketDataStoreReportContext(
        marketDataStore,
        curveIdentifier,
        environmentSliders.toList)
    }
  }
}

/**
 * Wraps the different PivotReport classes and runs a report (given a CurveIdentifier ReportOptions and UTPs)
 * Just returns the ReportData which is used in ReportPivotTableDataSource to create a pivot
 */
class PivotReportRunner(reportContextBuilder:ReportContextBuilder) {
  val marketSlideAttributes = Market.all.map(m => SlideAttributes(m.name, Some(m.standardShift.uom.toString), m.standardShift.value.toString))

  // We only want to slide commodities that have a standard futures market and all a commodities future markets have the same uom.
  val commodityMarketsWithFuture = Market.all.map(_.commodity).distinct.filter(Commodity.hasStandardFuturesMarket(_))
  val commodityToUOMMap = commodityMarketsWithFuture.foldLeft(Map[Commodity, ListBuffer[UOM]]())((map,commodity) => {
    val currentUOMs = map.getOrElse(commodity, new ListBuffer[UOM]())
    val thisUOM = Commodity.standardFuturesMarket(commodity).standardShift.uom
    currentUOMs += thisUOM
    map.updated(commodity, currentUOMs)
  })
  val validCommodityMarketsToSlide = commodityMarketsWithFuture.filterNot(c => commodityToUOMMap(c).distinct == 1).sortWith(_.name < _.name)
  val commodityMarketSlideAttributes = validCommodityMarketsToSlide.map(c =>
    SlideAttributes(c.name, Some(c.representativeMarket.standardShift.uom.toString), c.representativeMarket.standardShift.value.toString))

  val marketSlideAttributesForVol = Market.all.map(m => SlideAttributes(m.name, Some("%"), "1"))
  val commoditySlideAttributesForVol = validCommodityMarketsToSlide.map(c => SlideAttributes(c.name, Some("%"), "1"))
  val futuresMarketSlideAttributes = Market.futuresMarkets.map(m => SlideAttributes(m.name, Some(m.standardShift.uom.toString), m.standardShift.value.toString))
  val marketText = "Market:"
  val priceSlideParameters = SlideParametersAvailable(Price.toString, Some(marketSlideAttributes), "3", "3", None, None, None, marketText)
  val priceCommoditySlideParameters = SlideParametersAvailable(PriceCommodity.toString, None, "3", "3", None, None, Some(commodityMarketSlideAttributes), "Commodity:")
  val stressCommoditySlideParameters = SlideParametersAvailable(StressCommodity.toString, None, "3", "3", None, None, Some(commodityMarketSlideAttributes), "Commodity:")
  val volsSlideParameters = SlideParametersAvailable(Vols.toString, Some(marketSlideAttributesForVol), "3", "3", Some("1"), Some("%"), None, marketText)
  val volsCommoditySlideParameters = SlideParametersAvailable(VolsCommodity.toString, None, "3", "3", Some("1"), Some("%"), Some(commoditySlideAttributesForVol), marketText)
  val stdDevSlideParameters = SlideParametersAvailable(StdDev.toString, Some(futuresMarketSlideAttributes), "3", "3", Some("0.25"), None, None, marketText)
  // Spot fx
  val timeSlideParameters = SlideParametersAvailable(Time.toString, None, "3", "0", Some("1"), Some("Days"), None, "")

  // time doesn't work so I've removed it as an available slide. we'll have to fix it if someone asks for it.
  val availableSlideParameters = List(priceSlideParameters, priceCommoditySlideParameters, stressCommoditySlideParameters, volsSlideParameters, volsCommoditySlideParameters, stdDevSlideParameters/*, timeSlideParameters*/)
  val reportTypes: List[PivotReportType] = List(GreeksPivotReport, ThetaPivotReport, MtmPivotReport)
  val reportOptionsAvailable = ReportOptionsAvailable(reportTypes.map(_.label), availableSlideParameters)

  def runReport(curveIdentifier: CurveIdentifier, reportOptions: ReportOptions, utps : Map[UTPIdentifier, UTP]): ReportData = {
    val selectedReportTypes: List[PivotReportType] = reportTypes.filter(rt => reportOptions.containsReport(rt.name))
    val contextAndSideDetails: scala.List[(AbstractReportContext, SlideDetails)] = getContextAndSlideDetails(reportOptions, curveIdentifier)

    val reports: List[PivotReportData[_ <: PivotReportRow]] = selectedReportTypes.flatMap( reportType => {
      for ((context, slideDetails) <- contextAndSideDetails) yield {

        val singleReportData: PivotReportData[_ <: PivotReportRow] = runSingleReport(reportType, curveIdentifier.tradesUpToDay, context, slideDetails, utps)

        if (reportType == MtmPivotReport) {
          val mtmReportData = singleReportData.asInstanceOf[PivotReportData[Mtm]]
          mtmReportData.defaultCombinedRows//.foreach { row => mtmField.field.value(row) }
        }

        singleReportData
      }
    })

    def recorded = contextAndSideDetails.flatMap(_._1.recorded).toSet

    val errorInstrumentIDs = Set() ++ reports.flatMap(_.errorIDs)

    ReportData(reports, Map() ++ utps.filterKeys(errorInstrumentIDs.contains(_)), recorded)
  }

  private def runSingleReport(reportType: PivotReportType, tradesUpToDay:Day, context: AbstractReportContext,
    slideDetails:SlideDetails, utps: Map[UTPIdentifier, UTP]): PivotReportData[_ <: PivotReportRow] = {

    Log.infoWithTime[PivotReportData[_ <: PivotReportRow]]("Generate " + reportType.name) {
      val forwardUTPsIfNecessary = {
        if (tradesUpToDay < context.marketDay) {
          println("\n\n Moving utps forward to " + context.marketDay + " using env " + context.baseEnvironment.marketDay)
          utps.mapValues(_.forwardState(context.baseEnvironment, context.marketDayAndTime))
        } else {
          utps
        }
      }

      val report : PivotReport[_ <: PivotReportRow] = reportType.run(context, forwardUTPsIfNecessary)

      PivotReportData.run(report, forwardUTPsIfNecessary, slideDetails, reportType.availablePages, context.baseEnvironment.referenceDataLookup)
        .asInstanceOf[PivotReportData[_ <: PivotReportRow]] //compiler crashes without this cast
    }
  }

  private def getContextAndSlideDetails(
    reportOptions: ReportOptions, curveIdentifier: CurveIdentifier) : List[(AbstractReportContext, SlideDetails)] = {

    reportOptions.slides.map(ConvertedSlideParams.unlabel(_).environmentSliders) match {
      case Nil => {
        // We always want a normal context. We might want slide context's as well.
        List((reportContextBuilder.contextFromCurveIdentifier(curveIdentifier), SlideDetails.Null))
      }
      case List(environmentSliders) => {
        environmentSliders.map(println)
        // 1D slide.
        environmentSliders.map(es =>
          (reportContextBuilder.contextFromCurveIdentifier(curveIdentifier, es), SlideDetails.createFromSliders(es)))
      }
      case List(environmentSliders1, environmentSliders2) => {
        // 2D slide.
        for (es1 <- environmentSliders1; es2 <- environmentSliders2) yield
          (reportContextBuilder.contextFromCurveIdentifier(curveIdentifier, es1, es2), SlideDetails.createFromSliders(es1, es2))
      }
    }
  }
}

/**
 * Joins up trades and market data to create reports
 */
class ReportServiceInternal(reportContextBuilder:ReportContextBuilder, tradeStores: TradeStores, curveIdentifierFactory: CurveIdentifierFactory) {

  val pivotReportRunner = new PivotReportRunner(reportContextBuilder )

  private val cache = CacheFactory.getCache("StarlingServerImpl", unique = true)

  private val ytd = new YearToDateReport(reportContextBuilder, curveIdentifierFactory)

  def clearCache() {
    cache.clear
    ytd.clearCache
  }

  def singleTradeReport(trade: Trade, curveIdentifier: CurveIdentifier, reportSpecificChoices : ReportSpecificChoices): TradeValuation = {
    try {
      val defaultContext = reportContextBuilder.contextFromCurveIdentifier(curveIdentifier)
      import ReportSpecificOptions._
      val explanation = reportSpecificChoices.getOrElse(valuationCurrencyLabel, defaultLabel) match {
        case `defaultLabel` => trade.explain(defaultContext.environment)
        case UOM.Currency(ccy) => trade.explain(defaultContext.environment, ccy)
      }
      TradeValuation(Right(explanation))
    } catch {
      case t: Throwable => TradeValuation(Left(StackTrace(t)))
    }
  }

  def pnlReconciliation(curveIdentifier: CurveIdentifier, tradeSet: TradeSet, timestamp: Timestamp, eaiDB: DB): PivotTableDataSource = {
    val defaultContext = reportContextBuilder.contextFromCurveIdentifier(curveIdentifier)

    new PnLReconciliation(defaultContext, tradeSet, timestamp, eaiDB)
  }

  def reportErrors(reportParameters: ReportParameters):ReportErrors = {
    val reportData = createCachedReportData(reportParameters)
    ReportErrors(reportData.reports.flatMap(r=>r.errors.map { case(utpIdentifier,error)=>ReportError(utpIdentifier.id, reportData.errorUTPs(utpIdentifier).toString, error) }))
  }

  def createCachedReportData(reportParameters:ReportParameters):ReportData = {
    val allUtps = readUTPs(reportParameters)
    val utpsKey = allUtps.keySet
    cache.memoize((utpsKey, reportParameters.curveIdentifier, reportParameters.reportOptions), {
      pivotReportRunner.runReport( curveIdentifierFactory.unLabel(reportParameters.curveIdentifier), reportParameters.reportOptions, allUtps)
    })
  }

  def readUTPs(reportParameters:ReportParameters) = {

    val tradesUpToDay = reportParameters.curveIdentifier.tradesUpToDay
    val tradeSets: List[(TradeSet, Timestamp)] = if (reportParameters.runReports) {
      tradeStores.toTradeSets(reportParameters.tradeSelectionWithTimestamp)
    } else {
      List()
    }
    val tradeSetsKey = tradeSets.map(ts => (ts._1.key, ts._2)).toList
    def readUTPs = Map() ++ tradeSets.flatMap {
      case (tradeSet, timestamp) => {
        tradeSet.utps(tradesUpToDay, reportParameters.expiryDay, timestamp)
      }
    }
    cache.memoizeX((tradeSetsKey, reportParameters.expiryDay, tradesUpToDay, reportParameters.reportOptions), {readUTPs})
  }

  def reportPivot(reportParameters: ReportParameters, pivotFieldParams:PivotFieldParams) : PivotData = {
    cache.memoize( (reportParameters, pivotFieldParams), {
      val pivot = reportPivotTableDataSource(reportParameters)._2
      val pivotData = PivotTableModel.createPivotData(pivot, pivotFieldParams)

      val day1BookCloseValid = reportParameters.pnlParameters match {
        case Some(pnl) => pnl.tradeTimestampFrom match {
          case Some(t) if t.error != None => false
          case _ => true
        }
        case _ => true
      }
      val day2BookCloseValid = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp match {
        case Some((_,t)) if t.error != None => false
        case _ => true
      }
      val bookCloseValid = day1BookCloseValid && day2BookCloseValid

      if (reportParameters.runReports && bookCloseValid) {
        pivotData
      } else if (!reportParameters.runReports) {
        val pt = PivotTable.singleCellPivotTable("Click Run")
        pivotData.copy(pivotTable = pt)
      } else {
        val pt = PivotTable.singleCellPivotTable("Book close error")
        pivotData.copy(pivotTable = pt)
      }
    })
  }

  def recordedMarketDataReader(reportParameters: ReportParameters, dataTypes: MarketDataTypes) = {
    cache.memoize( ("recordMarketData", reportParameters), {
      val recorded = reportPivotTableDataSource(reportParameters)._1.map {
        case (observationPoint, key, data) => TimedMarketDataKey(observationPoint, key) → data
      }
      new RecordedMarketDataReader("Recorded from report using " + reportParameters.curveIdentifier.marketDataIdentifier, recorded.toList, dataTypes)
    } )
  }

  def reportPivotTableDataSource(reportParameters: ReportParameters) = cache.memoize((reportParameters), {
    val reportData:ReportData = createCachedReportData(reportParameters)
    createReportPivotDataSource(reportData, reportParameters)
  })

  def createReportPivotDataSource(reportData:ReportData, reportParameters: ReportParameters): (Set[(ObservationPoint, MarketDataKey, MarketData)], PivotTableDataSource) = {
    val tradeSets: List[(TradeSet, Timestamp)] = tradeStores.toTradeSets(reportParameters.tradeSelectionWithTimestamp)
    val pivots = if (reportParameters.reportOptions.isEmpty) List() else tradeSets.map {
      case (tradeSet, timestamp) => {
        val tradesPivot = tradeSet.reportPivot(
          reportParameters.curveIdentifier.tradesUpToDay, reportParameters.expiryDay,
          timestamp,
          reportParameters.runReports)
        val reports = reportData.reports
        val groupedReports = reports.groupBy(_.slideDetails.stepNumbers)
        val reportTableDataSources = groupedReports.keysIterator.map(stepNums => {
          (stepNums, new ReportPivotTableDataSource(tradesPivot, groupedReports(stepNums), reportParameters.desk))
        }).toList
        if (reportTableDataSources.size == 1) {
          reportTableDataSources(0)._2
        } else {
          def getSlideLabel(label:SlideParametersLabel) = {
            label.market match {
              case None => label.commodity match {
                case None => label.slideType
                case Some(c) => c
              }
              case Some(mar) => mar
            }
          }
          val slideLabels = reportParameters.reportOptions.slide1 match {
            case None => List()
            case Some(slide1Label) => {
              reportParameters.reportOptions.slide2 match {
                case None => {
                  List(getSlideLabel(slide1Label))
                }
                case Some(slide2Label) => {
                  List(getSlideLabel(slide1Label), getSlideLabel(slide2Label))
                }
              }
            }
          }
          new SlideReportTableDataSource(reportTableDataSources, slideLabels)
        }
      }
    }
    val pnlRecordedPlusPivots = reportParameters.pnlParameters match {
      case None => List()
      case Some(pnlFromParameters) => {
        for ((tradeSet,timestamp) <- tradeSets) yield {

          // the from timesetamp sometimes isn't defined, in that case we want to use the 'to' timestamp instead.
          val fromTimestamp = pnlFromParameters.tradeTimestampFrom match {
            case Some(ts) => tradeSet.tradeSystem match {
              case IntradayTradeSystem => new Timestamp(0) // So blotter trades are always in 'created' state
              case _ => ts.timestamp
            }
            case None => timestamp
          }
          pnlReport(
            tradeSet,
            curveIdentifierFactory.unLabel(pnlFromParameters.curveIdentifierFrom), curveIdentifierFactory.unLabel(reportParameters.curveIdentifier),
            fromTimestamp, timestamp,
            reportParameters.expiryDay,
            reportParameters.runReports)
        }
      }
    }
    val yearToDates = if (reportParameters.runReports) {
      ytd.report(tradeSets, reportParameters)
    } else {
      Nil
    }
    val allRecorded = reportData.recorded ++ pnlRecordedPlusPivots.flatMap(_._1)
    val allPivots = pivots ::: pnlRecordedPlusPivots.map(_._2) ::: yearToDates
    if (allPivots.isEmpty)
      (allRecorded, NullPivotTableDataSource)
    else
      (allRecorded, UnionPivotTableDataSource.join(allPivots))
  }

  private def pnlReport(
    tradeSet: TradeSet,
    curveIdentifierDm1: CurveIdentifier,
    curveIdentifierD: CurveIdentifier,
    tM1: Timestamp,
    t: Timestamp,
    expiryDay: Day, addRows:Boolean) = {

    def op = {
      val c1 = reportContextBuilder.contextFromCurveIdentifier(curveIdentifierDm1)
      val c2 = reportContextBuilder.contextFromCurveIdentifier(curveIdentifierD)

      val d1 = c1.environment.atomicEnv
      val d2 = c2.environment.atomicEnv

      val pivot = PnlExplanationReport.run(d1, d2, tradeSet, curveIdentifierDm1, curveIdentifierD, tM1, t, expiryDay, addRows)
      (c1.recorded ++ c2.recorded, pivot)
    }
    val key = List(tradeSet.key, curveIdentifierDm1, curveIdentifierD, t, tM1, expiryDay)
    cache.memoize((key), op)
  }
}

class ConvertedSlideParams(slideType: SlideType, market: Option[CommodityMarket], stepSize: Double, uom: Option[UOM],
                           stepIndexList: List[Int], commodity:Option[Commodity]) {

  def environmentSliders : List[EnvironmentSlider] = stepIndexList.map { stepNumber => slideType match {
    case Price => PriceSlideParameters(market.get, Quantity(stepSize * stepNumber, uom.get), stepNumber)
    case PriceCommodity => PriceCommoditySlideParameters(commodity.get, Quantity(stepSize * stepNumber, uom.get), stepNumber)
    case StressCommodity => StressCommoditySlideParameters(commodity.get, Quantity(stepSize * stepNumber, uom.get), stepNumber)
    case Vols => VolsSlideParameters(market.get, Percentage(stepSize / 100.0 * stepNumber), stepNumber)
    case VolsCommodity => VolsCommoditySlideParameters(commodity.get, Percentage(stepSize / 100.0 * stepNumber), stepNumber)
    case StdDev => {
      assert(market.get.isInstanceOf[FuturesMarket], market + " is not a futures market")
      StdDevSlideParameters(market.get.asInstanceOf[FuturesMarket], Quantity(stepSize * stepNumber, uom.get), stepNumber)
    }
    case Time => TimeSlideParameters(stepSize.toInt * stepNumber, stepNumber)
  }}
}

object ConvertedSlideParams {
  def unlabel(params: SlideParametersLabel): ConvertedSlideParams = {
    val slideType = SlideType.withName(params.slideType)
    val stepIndexList = (-params.downSteps to params.upSteps).toList

    val uom = slideType match {
      case Price => Some(UOM.fromString(params.uom))
      case PriceCommodity => Some(UOM.fromString(params.uom))
      case StressCommodity => Some(UOM.fromString(params.uom))
      case Vols => None
      case VolsCommodity => None
      case StdDev => Some(UOM.fromString(params.uom))
      case Time => None
    }

    val market = params.market.map(mar => Market.fromName(mar))
    val commodity = params.commodity.flatMap(com => Commodity.fromNameOption(com))
    new ConvertedSlideParams(slideType, market, params.stepSize, uom, stepIndexList, commodity)
  }
}

