package starling.reports.pivot

import greeks._
import starling.marketdata.MarketData
import starling.curves._
import starling.instrument._
import java.lang.Throwable
import starling.pivot._
import starling.quantity.UOM
import starling.daterange.{Period, Day}
import starling.reports.pivot.PivotReport._
import starling.utils.cache.CacheFactory
import starling.daterange.DayAndTime
import starling.concurrent.MP._
import starling.tradestore.{TradeAndFields, TradeChanges}
import starling.daterange.TimeOfDay
import starling.trade.Trade
import starling.utils.{AppendingMap, AtomicDatumKeyUtils}
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.daterange.Month

object MarketChangesRiskFields extends RiskPivotFields[MarketChangesPnlRow]

/**
 * Given two environments the pnl is broken down by curve key (eg WTI vols, USD discount, WTI prices)
 * The sum of the pnl components is always the same as the vanilla pnl
 * This is achieved with the addition of two extra pnl components:
 *   cross terms: the difference between the sum of the curve key values and all the curve values
 *   other changes: the difference between the sum of the curve keys values and the vanilla pnl
 */
class MarketChangesPnl(d1: AtomicEnvironment, d2: AtomicEnvironment, utps : Map[UTPIdentifier, UTP]) extends PivotReport[MarketChangesPnlRow] {
  private val d1EnvFwd = Environment(d1)
  private val d2Env = Environment(d2)

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  val environmentForCache = CacheFactory.getCache("MarketChangesPnl.environmentFor", unique = true)

  private def environmentFor(curveKeys: Set[CurveKey]) = {
    environmentForCache.memoize(
      (curveKeys),
      {
        Environment(OverrideForCurveKeysEnvironment(d1, curveKeys, d2))
      }
      )
  }

  def fields = List(
    new PivotReportField[MarketChangesPnlRow]("Day Change Type") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.groupName
    },
    new PivotReportField[MarketChangesPnlRow]("Day Change Component") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.componentName
    },
    new PivotReportField[MarketChangesPnlRow]("Day Change") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.pnl * reportRow.scale

      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    },
    new PivotReportField[MarketChangesPnlRow]("Price Change") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.priceChange match {
        case None => RiskPrices.Null
        case Some(pq) => new RiskPrices(reportRow.marketName, reportRow.period, pq)
      }

      override def pivotFieldDetails = new AveragePriceFieldDetails(name)
    },
    new PivotReportField[MarketChangesPnlRow]("D1 Price") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.d1Price match {
        case None => RiskPrices.Null
        case Some(pq) => new RiskPrices(reportRow.marketName, reportRow.period, pq)
      }

      override def pivotFieldDetails = new AveragePriceFieldDetails(name)
    },
    new PivotReportField[MarketChangesPnlRow]("Vol Change") {
      def value(reportRow: MarketChangesPnlRow) = reportRow.volChange match {
        case None => RiskVols.Null
        case Some(pq) => pq.quantityValue.map{v => {
          new RiskVols(reportRow.marketName, reportRow.period, VolatilityMeasure(v))
        }}.getOrElse(RiskVols.Null)
      }
      override def pivotFieldDetails = new AverageVolatilityFieldDetails(name)
    }
  ) ::: MarketChangesRiskFields.riskFields

  def scale(row: MarketChangesPnlRow, volume: Double) = row * volume

  private def derivNameAndType(curveKey: CurveKey, order: Option[Int]): (String, String) = (curveKey, order) match {
    case (_: SpreadAtmStdDevCurveKey, None) => ("Spread StdDev Delta", "Vol")
    case (_: ForwardCurveKey, None) => ("Delta", "Price")
    case (_: OilAtmVolCurveKey, None) => ("Vega", "Vol")
    case (_: DiscountCurveKey, None) => ("Rho", "Time")
    case (_: FixingsHistoryKey, None) => ("Fixings", "Price")
    case (k, None) => (curveKey.typeName, curveKey.typeName)

    case (_: ForwardCurveKey, Some(1)) => ("Delta", "Price")
    case (_: OilAtmVolCurveKey, Some(1)) => ("Vega", "Vol")
    case (_: BradyMetalVolCurveKey, Some(1)) => ("Vega", "Vol")
    case (_: USDFXRateCurveKey, Some(1)) => ("USD FX Delta", "FX")
    case (_: SpreadAtmStdDevCurveKey, Some(1)) => ("Spread StdDev Delta", "Vol")
    case (_: OilVolSkewCurveKey, Some(1)) => ("Vol skew Delta", "Vol")

    case (_: ForwardCurveKey, Some(2)) => ("Gamma", "Price")
    case (_: OilAtmVolCurveKey, Some(2)) => ("Vomma", "Vol")
    case (_: BradyMetalVolCurveKey, Some(2)) => ("Vomma", "Vol")
    case (_: USDFXRateCurveKey, Some(2)) => ("USD FX Gamma", "FX")
    case (_: SpreadAtmStdDevCurveKey, Some(2)) => ("Spread StdDev Gamma", "Vol")
    case (_: OilVolSkewCurveKey, Some(2)) => ("Vol skew Gamma", "Vol")
  }

  val ignore:Set[Class[_]] = Set(classOf[DiscountRateKey], classOf[FixingKey])

  def rows(utpID: UTPIdentifier, instrument: UTP) = {
    List(MarketChangesPnlRow(utpID, instrument, groupName = null, componentName = null))
  } 

  private def environmentDiffsAndCurveKeys(utp : UTP, reportSpecificChoices : ReportSpecificChoices) : (Set[EnvironmentDifferentiable], Set[CurveKey]) = {
    val curveKeys = AtomicDatumKeyUtils.curveKeys(utp, d1EnvFwd.marketDay, UOM.USD)
    val (priceKeys, volKeys) = PivotReportUtils.priceAndVolKeys(utp, d1EnvFwd.marketDay, reportSpecificChoices)
    val envDiffs = priceKeys ++ volKeys
    (envDiffs, curveKeys)
  }

  override def combine(rows : List[MarketChangesPnlRow], reportSpecificChoices : ReportSpecificChoices) = {
//    require(!reportSpecificChoices.getOrElse(futuresAsSwaps_str, false), "Can't do day change report with futures as swaps")
//    require(!reportSpecificChoices.getOrElse(futuresAsSpreads_str, false), "Can't do day change report with futures as spreads")

    val showEqFutures = reportSpecificChoices.getOrElse(showEqFutures_str, false)
    val useSkew = reportSpecificChoices.getOrElse(useSkew_str, true)
    val collapseOptions = reportSpecificChoices.getOrElse(collapseOptions_str, true)
    val atmVega = reportSpecificChoices.getOrElse(atmVega_str, false)
    val env = if (useSkew) d1EnvFwd else d1EnvFwd.setShiftsCanBeIgnored(true)
    val riskInstruments = new PivotUTPRestructurer(env, reportSpecificChoices, spreadMonthsByStrategyAndMarket, swapIndices).apply(rows.map{r => UTPWithIdentifier(r.utpID, r.utp * r.scale)})

    val combinedRows: List[MarketChangesPnlRow] = riskInstruments.mpFlatMap{
      case UTPWithIdentifier(utpID, utp) => 
        val (unitUTP, volume) = utp.asUnitUTP

        val (envDiffs, curveKeys) = environmentDiffsAndCurveKeys(unitUTP, reportSpecificChoices)
        val pnlBreakDown = unitUTP.explain(d1EnvFwd, d2Env, environmentFor, UOM.USD, envDiffs, curveKeys, atmVega = atmVega)
        val explainedTotal = pnlBreakDown.map {_.value}.toList.sum

        val (crossTerms, rounding, unexplained) = unitUTP.components(d1EnvFwd, d2Env, environmentFor, UOM.USD, explainedTotal, curveKeys)

        def makeRow(groupName: String,
          componentName : String, riskType : Option[String], 
          riskCommodity : Option[String], marketName : String, period : Option[Period], 
          pnl : PivotQuantity, priceChange : Option[PivotQuantity], d1Price : Option[PivotQuantity], volChange : Option[PivotQuantity]) = {
          MarketChangesPnlRow(
            utpID, utp,
            groupName = groupName,
            componentName = componentName, 
            riskType = riskType,
            riskCommodity = riskCommodity,
            marketName = marketName, 
            period = period, 
            pnl = pnl, 
            priceChange = priceChange,
            d1Price = d1Price,
            volChange = volChange,
            collapseOptions = collapseOptions, 
            scale = volume
          )
        }
        
        val (priceDiffs, volDiffs) = PivotReportUtils.priceAndVolKeys(unitUTP, d1EnvFwd.marketDay, reportSpecificChoices)
        val diffs = (priceDiffs ++ volDiffs).toList

        def shareAcrossDiff(groupName: String, name : String, value : PivotQuantity) : List[MarketChangesPnlRow] = {
          if (diffs.isEmpty)
            List(makeRow(groupName, name, None, None, "", None, value, priceChange = None, d1Price = None, volChange = None))
          else
            diffs.map {
              diff => makeRow(groupName, name, None, Some(diff.riskCommodity), diff.riskMarket, diff.periodKey, value / diffs.size,
                priceChange = None, d1Price = None, volChange = None)
            }
        }
        val crossTermComponents = shareAcrossDiff("Cross Terms", "Cross Terms", crossTerms)
        val roundingComponents = shareAcrossDiff("Price", "Rounding", rounding)

        var pnlComponents =
          makeRow("Other", "Other changes", None, None, "", None, unexplained, None, None, None) ::
          roundingComponents ::: crossTermComponents :::
          pnlBreakDown.flatMap{
            // Split discount and fixing pnl across markets and periods
            explanation =>
              val (n, group) = derivNameAndType(explanation.curveKeys.head, explanation.order)
              val name = n + " DC"
              explanation.curveKeys.head match {
                case _: DiscountCurveKey => shareAcrossDiff(group, name, explanation.value)
                case _: FixingsHistoryKey => shareAcrossDiff(group, name, explanation.value)
                case _ => List(makeRow(group, name, explanation.riskType, explanation.riskCommodity, explanation.riskMarket,
                                        explanation.period, explanation.value, explanation.priceChange, explanation.d1Price, explanation.volChange))
              }
          }
        pnlComponents = pnlComponents.filter(!_.pnl.isAlmostZero)
        pnlComponents = pnlComponents.map(_.setPeriodForDisplay(reportSpecificChoices.getOrElse(tenor_str, Month)))
        val pnlComponentsByCashOrNot = pnlComponents.groupBy{
          _.utp match {
            case _ : BankAccount => "CASH"
            case _ : CashInstrument => "CASH"
            case _ => "OTHER"
          }
        }
        val cashComponents = pnlComponentsByCashOrNot.getOrElse("CASH", Nil).flatMap(_.splitByEnvironmentDifferentiable(d1EnvFwd.marketDay, reportSpecificChoices))
        val nonCashComponents = pnlComponentsByCashOrNot.getOrElse("OTHER", Nil)
        cashComponents ::: nonCashComponents
    }
    combinedRows
  }
  override def reportSpecificOptions = {
    super.reportSpecificOptions :+ 
      (atmVega_str -> List(false, true))
  }
}

case class MarketChangesPnlRow(
  utpID : UTPIdentifier,
  utp : UTP,
  groupName: String,
  componentName: String, 
  marketName: String = "", 
  riskType : Option[String] = None,
  riskCommodity : Option[String] = None,
  period: Option[Period] = None, 
  pnl: PivotQuantity = PivotQuantity.NULL,
  priceChange : Option[PivotQuantity] = None,
  d1Price : Option[PivotQuantity] = None,
  volChange : Option[PivotQuantity] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
) 
  extends RiskPivotReportRow[MarketChangesPnlRow] with PivotRowShareableByRiskFactor[MarketChangesPnlRow]
{
  def *(volume: Double) = copy(scale = scale * volume)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(
    marketName = diff.riskMarket,
    riskType = Some(diff.riskType),
    riskCommodity = Some(diff.riskCommodity),
    period = diff.periodKey
  )
}

case class TimeChangesPnlRow(
  utpID : UTPIdentifier,
  utp : UTP,
  label: String,
  pnl: PivotQuantity,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
) 
  extends PivotRowShareableByRiskFactor[TimeChangesPnlRow] with PivotRowWithEnvironmentDifferentiable[TimeChangesPnlRow]
{
  def groupName: String = "Time"

  def *(volume: Double) = copy(scale = scale * volume)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
  def scaledPnl = pnl * scale
}

class TimeChangesPnl(d1:Environment, forwardDay:Day, utps : Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[TimeChangesPnlRow] {

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  def fields = List(
    new PivotReportField[TimeChangesPnlRow]("Day Change Type") {
      def value(reportRow: TimeChangesPnlRow) = reportRow.groupName
    },
    new PivotReportField[TimeChangesPnlRow]("Day Change Component") {
      def value(reportRow: TimeChangesPnlRow) = reportRow.label
    },
    new PivotReportField[TimeChangesPnlRow]("Day Change") {
      def value(reportRow: TimeChangesPnlRow) = reportRow.scaledPnl

      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    }
    ) ::: TimeChangesRiskFields.riskFields

  def scale(row: TimeChangesPnlRow, volume: Double) = row * volume

  def rows(utpID : UTPIdentifier, utp:UTP) = {
    List(TimeChangesPnlRow(utpID, utp, "", PivotQuantity.NULL))
  }

  override def combine(rows : List[TimeChangesPnlRow], reportSpecificChoices : ReportSpecificChoices) = {
    val atmVega = reportSpecificChoices.getOrElse(atmVega_str, false)
    val changeOnlyTimeAndDiscounts = !atmVega

    import starling.concurrent.MP._
    val (_, d1Fwd) = starling.instrument.Greeks.envFor(d1, forwardDay.atTimeOfDay(TimeOfDay.EndOfDay), changeOnlyTimeAndDiscounts)

    val combinedRows = new PivotUTPRestructurer(d1Fwd, reportSpecificChoices, spreadMonthsByStrategyAndMarket, swapIndices).transformUTPs(rows).mpFlatMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP
        val d1Value = PivotQuantity.calcOrCatch(unitUTP.cachedMtm(d1, UOM.USD))
        val d1FwdValue = PivotQuantity.calcOrCatch(unitUTP.cachedMtm(d1Fwd, UOM.USD))
        val d1FwdValueFwdInstrument = PivotQuantity.calcOrCatch(unitUTP.forwardState(d1, d1Fwd.marketDay).cachedMtm(d1Fwd, UOM.USD))
        val totalChange = d1FwdValue - d1Value //eg. 0 - value of the futures option
        val theta = d1FwdValueFwdInstrument - d1Value  //eg. value of the future exercised into - value of the futures option
        val expiry = totalChange - theta
        List(
          TimeChangesPnlRow(utpID, utp, "Theta DC", theta),
          TimeChangesPnlRow(utpID, utp, "Expiry DC", expiry)
        ).filterNot(_.pnl.isAlmostZero).map (_ * volume)
      }
    }
    super.combine(combinedRows, reportSpecificChoices)
  }
  def marketDay = d1.marketDay
  override def reportSpecificOptions = super.reportSpecificOptions :+ 
      (atmVega_str -> List(false, true))
}

object TimeChangesRiskFields extends RiskPivotFields[TimeChangesPnlRow]

case class TradeChangesPnlRow(
  trade: TradeAndFields,
  label: String, 
  pnl: PivotQuantity,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  scale : Double = 1.0
){
  def riskMarket : String = diff match {
    case Some(d) => d.riskMarket
    case None => ""
  }
  def riskCommodity : String = diff match {
    case Some(d) => d.riskCommodity
    case None => ""
  }
  def riskType : String = diff match {
    case Some(d) => d.riskType
    case None => ""
  }
}

class TradeChangesPnl(d1: Environment) {
  def rows(tradeChanges: TradeChanges): List[TradeChangesPnlRow] = {
    val r = tradeChanges.created.map {t => TradeChangesPnlRow(t, "Created", PivotQuantity.calcOrCatch({t.trade.cachedMtm(d1, UOM.USD)}))} :::
            tradeChanges.deleted.map {t => TradeChangesPnlRow(t, "Deleted", PivotQuantity.calcOrCatch({t.trade.cachedMtm(d1, UOM.USD).negate}))} :::
            tradeChanges.movedIn.map {t => TradeChangesPnlRow(t, "Moved In", PivotQuantity.calcOrCatch({t.trade.cachedMtm(d1, UOM.USD)}))} :::
            tradeChanges.movedOut.map {t => TradeChangesPnlRow(t, "Moved Out", PivotQuantity.calcOrCatch({t.trade.cachedMtm(d1, UOM.USD).negate}))} :::
            tradeChanges.amended.map {case (a, b) => TradeChangesPnlRow(b, "Amended", PivotQuantity.calcOrCatch({b.trade.cachedMtm(d1, UOM.USD) - a.trade.cachedMtm(d1, UOM.USD)}))}
    r.filterNot(_.pnl.isAlmostZero)
  }
}

class TradeChangesPnlPivotTableDataSource(tradeChangesFields:List[FieldDetailsGroup], marketDay : DayAndTime, pnlDetails: List[TradeChangesPnlRow]) extends UnfilteredPivotTableDataSource {
  val pnlComponentField = Field("Day Change Component")
  val pnlField = Field("Day Change")
  val riskMarketField = Field("Risk Market")
  val riskCommodityField = Field("Risk Commodity")
  val riskTypeField = Field("Risk Type")
  val riskPeriodField = Field("Risk Period")
  val fieldDetailsGroups =
    FieldDetailsGroup(
      "Report Fields", 
      new SumPivotQuantityFieldDetails("Day Change"),
      FieldDetails("Day Change Component"),
      FieldDetails("Risk Market"),
      FieldDetails("Risk Commodity"),
      FieldDetails("Risk Type"),
      new FieldDetails("Risk Period"){
        override def nullValue = OptionalPeriodLabel.Null
      }
    ) :: tradeChangesFields

  private val fieldsByName = Map() ++ fieldDetails.map(f => f.field.name -> f.field)


  def unfilteredData(pfs: PivotFieldsState) = {
    def priceAndVolKeys(trade : Trade) = {
      trade.asUtpPortfolio.portfolio.keySet.toList.flatMap{
        utp =>
          val (priceKeys, volKeys) = PivotReportUtils.priceAndVolKeys(utp, Day(2000, 1, 1).startOfDay,
            ReportSpecificChoices.create(pfs.reportSpecificChoices.toMap))
          (priceKeys ++ volKeys).toList
      }
    }
    val resultData = {
      // Break out the trade changes pnl by risk market and period
      pnlDetails.flatMap{
        details =>
          val pAndVKeys = priceAndVolKeys(details.trade.trade)
          if (pAndVKeys.isEmpty)
            List(details)
          else
            pAndVKeys.map{
              diff => details.copy(diff = Some(diff), period = diff.periodKey, scale = details.scale / pAndVKeys.size)
            }
      }.map {
        details => {
          new AppendingMap(details.trade.fields.maps + ("sd" -> Map(
            pnlComponentField -> details.label, 
            pnlField -> details.pnl * details.scale,
            riskTypeField -> details.riskType,
            riskCommodityField -> details.riskCommodity,
            riskMarketField -> details.riskMarket,
            riskPeriodField -> OptionalPeriodLabel(details.period)
          )))
        }
      }
    }
    resultData
  }
}

case class NewTradesRow(
  utpID : UTPIdentifier,
  utp : UTP,
  mtm: PivotQuantity,
  diff : Option[EnvironmentDifferentiable] = None,
  period : Option[Period] = None,
  collapseOptions : Boolean = true,
  scale : Double = 1.0
) 
  extends PivotRowWithEnvironmentDifferentiable[NewTradesRow] with PivotRowShareableByRiskFactor[NewTradesRow] 
{
  def *(volume: Double) = copy(scale = scale * volume)
  def setPeriod(period : Option[Period]) = copy(period = period)
  def setCollapseOptions(co : Boolean) = copy(collapseOptions = co)
  def setDiff(diff : EnvironmentDifferentiable) = copy(diff = Some(diff), period = diff.periodKey)
}

object NewTradesRiskFields extends RiskPivotFields[NewTradesRow]

class NewTradesPivotReport(environment: Environment, currency: UOM, utps : Map[UTPIdentifier, UTP]) extends RiskFactorSplittingPivotReport[NewTradesRow] {
  def marketDay = environment.marketDay

  val spreadMonthsByStrategyAndMarket = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  def fields = List(
    new PivotReportField[NewTradesRow]("Day Change Type") {
      def value(reportRow: NewTradesRow) = "New Trades"
    },
    new PivotReportField[NewTradesRow]("Day Change Component") {
      def value(reportRow: NewTradesRow) = "New Trades"
    },
    new PivotReportField[NewTradesRow]("Day Change") {
      def value(reportRow: NewTradesRow) = reportRow.mtm * reportRow.scale

      override def pivotFieldDetails = new SumPivotQuantityFieldDetails(name)
    }
  ) ::: NewTradesRiskFields.riskFields

  def rows(utpID: UTPIdentifier, instrument: UTP) = List(
    NewTradesRow(utpID, instrument, PivotQuantity.NULL)
  )

  override def combine(rows : List[NewTradesRow], reportSpecificChoices : ReportSpecificChoices) : List[NewTradesRow] = {
    import starling.concurrent.MP._

    val combinedRows = new PivotUTPRestructurer(environment, reportSpecificChoices, spreadMonthsByStrategyAndMarket, swapIndices).transformUTPs(rows).mpMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP
        NewTradesRow(utpID, utp, PivotQuantity.calcOrCatch(unitUTP.cachedMtm(environment, UOM.USD))) * volume
      }
    }
    super.combine(combinedRows, reportSpecificChoices)
  }
  def scale(row: NewTradesRow, volume: Double) = row * volume
}

class TradeChangesPivotTableDataSource(tradeChanges: TradeChanges) extends UnfilteredPivotTableDataSource {
  override def initialState = {
    val columns = fieldDetails.map(_.field).filter(f => f.name != "Trade ID" && f.name != "Action").map(field => ColumnTree(field, true))
    new PivotFieldsState(
      rowFields = List(Field("Trade ID"), actionField),
      columns = ColumnTrees(columns)
    )
  }

  val actionField = Field("Action")

  val fieldDetailsGroups = FieldDetailsGroup("Action Fields", List(FieldDetails(actionField))) :: tradeChanges.fields

  private val fieldsByName = Map() ++ fieldDetails.map(f => f.field.name -> f.field)

  def unfilteredData(pfs: PivotFieldsState) = {
    def buildRow(action: String, trade: TradeAndFields) = {
      Map(actionField -> action) ++ trade.fields
    }
    val resultData =
    tradeChanges.movedIn.map(trade => buildRow("Moved In", trade)) :::
            tradeChanges.movedOut.map(trade => buildRow("Moved Out", trade)) :::
            tradeChanges.created.map(trade => buildRow("Created", trade)) :::
            tradeChanges.deleted.map(trade => buildRow("Deleted", trade)) :::
            tradeChanges.undeleted.map(trade => buildRow("Undeleted", trade)) :::
            tradeChanges.amended.flatMap {case (t1, t2) => List(buildRow("Amended From", t1), buildRow("Amended To", t2))}
    resultData
  }
}


