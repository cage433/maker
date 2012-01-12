package starling.gui

import api._
import pages._
import swing._
import event._
import collection.immutable.TreeSet
import starling.daterange._
import starling.gui.StarlingLocalCache._
import starling.browser.PageContext
import starling.browser.common.GuiUtils._
import starling.browser.common.{ResizingLabel, RoundedBorder, MigPanel}
import java.awt.Font

class ManualReportConfigPanel(context:PageContext, reportParameters:ReportParameters, pivotPageState:PivotPageState)
        extends MigPanel with ConfigPanel {
  def displayName = "Manual"

  val tradeSel = reportParameters.tradeSelectionWithTimestamp.asTradeSelection
  val initialTradesAsOf = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map(_._2)

  val pricingGroups = context.localCache.pricingGroups(tradeSel.desk)
  val pricingGroupPanel = new MarketDataSelectionComponent(
    context,
    reportParameters.tradeSelectionWithTimestamp.desk,
    reportParameters.curveIdentifier.marketDataIdentifier, showShapshotChooser = true) {
    reactions += { case MarketDataSelectionChanged(selection) => {
      ManualReportConfigPanel.this.publish(MarketDataChanged(ManualReportConfigPanel.this))
      updateSnapshotButton()
    } }
  }
  val pricingGroupPanelPanel = new MigPanel {
    val snapshotButton = new Button {
      tooltip = "Import market data for the Curve Data day selected (and all from LIM)"
      icon = StarlingIcons.icon("/icons/14x14_download_data.png")
      reactions += {
        case ButtonClicked(_) => {
          context.submit(ImportMarketDataRequest(generateMarketDataIdentifier.selection, curveDataDayAndTimeChooser.day))
        }
      }
    }
    border = RoundedBorder(colour = PivotTableBackgroundColour)
    add(pricingGroupPanel, "push, grow")
    add(snapshotButton, "gapleft rel, ay top")
  }

  val optionsPanel = new MigPanel {
    border = RoundedBorder(PivotTableBackgroundColour)
    val discountedCheckbox = new CheckBox("Discounted") {
      tooltip = "Select this to run reports with discounting applied"
    }
    val zeroVolsCheckbox = new CheckBox("Zero Vols") {
      tooltip = "Select this to run reports with all volatilities set to 0%"
    }

    add(discountedCheckbox, "wrap")
    add(zeroVolsCheckbox)

    reactions += {
      case ButtonClicked(_) => updateRunButton()
    }
    listenTo(discountedCheckbox, zeroVolsCheckbox)
  }

  val curveDataDayAndTimeChooser = new DayAndTimeChooser()

   /**
   * The panel for all the day 2 valuation fields. In the context where pnl is not chosen, d2 is the only active valuation information.
   */
  val day2Panel = new MigPanel(columnConstraints = "[p][p][p]unrel[p][p]unrel[p][p]") {
    border = RoundedBorder(PivotTableBackgroundColour)

    val environmentRuleLabel = new ResizingLabel("Env Rule:") {
      tooltip = "The rule for selecting and deriving curves from market data"
    }
    val environmentRule = new EnvironmentRuleChooser(
      reportParameters.curveIdentifier.environmentRule,
      context.localCache.environmentRulesForPricingGroup(reportParameters.curveIdentifier.marketDataIdentifier.selection.pricingGroup)
    )

    val marketDayLabel = new ResizingLabel("Market Day:") {
      tooltip = "Valuations need the 'current' day, in particular for discounts, forward FX, vols. Also trades with a trade day after this day are ignored. This day can not be before the curve data day."
    }
    val marketDayAndTimeChooser = new DayAndTimeChooser(timeOfDay0 = TimeOfDay.StartOfDay) {
      leftDisabledTooltip = "The market day cannot be before the curve data day"
    }

    val thetaToLabel = new ResizingLabel("Theta to:") {
      tooltip = "Theta is calculated as the change between the market day and this day (usually the business day after the curve data day)"
    }
    val thetaToDayChooser = new DayChooser() {
      leftDisabledTooltip = "The theta day must be at least one day after the market day"
    }

    val curveDataLabel = new ResizingLabel("Curve Data:") {
      tooltip = "Market data from this day will be used in valuations"
      font = font.deriveFont(Font.BOLD)
    }

    val liveOnLabel = new ResizingLabel("Live on:") {
      tooltip = "Trades that expire before this day are excluded"
    }
    val liveOnDayChooser = new DayChooser(enableFlags = false)
    
    val bookCloseLabel = new ResizingLabel("Book close:") {
      tooltip = "The trades used in the report as they were at this point"
    }
    val bookCloseChooser = new TimestampChooser(initialTradesAsOf, tradeSel.desk, context)

    add(curveDataLabel)
    add(curveDataDayAndTimeChooser)
    add(curveDataDayAndTimeChooser.timeOfDayChooser)
    add(environmentRuleLabel)
    add(environmentRule)
    add(thetaToLabel)
    add(thetaToDayChooser, "wrap")

    add(marketDayLabel)
    add(marketDayAndTimeChooser)
    add(marketDayAndTimeChooser.timeOfDayChooser)
    add(bookCloseLabel)
    add(bookCloseChooser)
    add(liveOnLabel)
    add(liveOnDayChooser)

    def updatePopulatedDays(selection:MarketDataSelection=pricingGroupPanel.selection.selection) {
      val flaggedDays = context.localCache.populatedDays(selection).toSet
      curveDataDayAndTimeChooser.flagged = flaggedDays
      marketDayAndTimeChooser.flagged = flaggedDays
      thetaToDayChooser.flagged = flaggedDays
    }

    def updateMarketDayLeftEnabled() {
      marketDayAndTimeChooser.leftEnabled = marketDayAndTimeChooser.day > curveDataDayAndTimeChooser.day
      marketDayAndTimeChooser.timeOfDayEnabled =
        (marketDayAndTimeChooser.day > curveDataDayAndTimeChooser.day) ||
          curveDataDayAndTimeChooser.timeOfDay == TimeOfDay.StartOfDay
      if (marketDayAndTimeChooser.day == curveDataDayAndTimeChooser.day &&
          curveDataDayAndTimeChooser.timeOfDay == TimeOfDay.EndOfDay) {
        marketDayAndTimeChooser.timeOfDay = TimeOfDay.EndOfDay
      }
    }
    def updateThetaLeftEnabled() {
      thetaToDayChooser.leftEnabled = thetaToDayChooser.day > marketDayAndTimeChooser.day.nextDay
    }

    reactions += {
      case DayAndTimeChangedEvent(`curveDataDayAndTimeChooser`, dayAndTime, previousDayAndTime) => {
        if (marketDayAndTimeChooser.day == previousDayAndTime.day) {
          val d = dayAndTime.day
          val timeOfDayToUse = if ((d >= Day.today) && (environmentRule.rule == EnvironmentRuleLabel.RealTime)) TimeOfDay.StartOfDay else TimeOfDay.EndOfDay
          marketDayAndTimeChooser.dayAndTime = d.atTimeOfDay(timeOfDayToUse)
        }
        updateMarketDayLeftEnabled()
      }
      case DayAndTimeChangedEvent(`marketDayAndTimeChooser`, dayAndTime, previousDayAndTime) => {
        val d = dayAndTime.day
        if (thetaToDayChooser.day == previousDayAndTime.day.nextBusinessDay(context.localCache.ukBusinessCalendar)) {
          thetaToDayChooser.day = d.nextBusinessDay(context.localCache.ukBusinessCalendar)
        }
        if (liveOnDayChooser.day == previousDayAndTime.day && liveOnDayChooser.day != d.startOfFinancialYear) {
          liveOnDayChooser.day = d
        }
        updateMarketDayLeftEnabled()
        updateThetaLeftEnabled()
      }
      case DayChangedEvent(`thetaToDayChooser`, _, _) => {
        updateThetaLeftEnabled()
      }
      case EnvironmentRuleLabelChangedEvent(_, _) => updateRunButton()
      case ExcelObservationDay(_, _) | PricingGroupObservationDay(_, _) => updatePopulatedDays()
      case DayChangedEvent(_,_,_) => updateRunButton()
      case SelectionChanged(`bookCloseChooser`) => updateRunButton()
      case MarketDataSelectionChanged(mdi) => {
        updatePopulatedDays(mdi.selection)
      }
    }

    listenTo(pricingGroupPanel, curveDataDayAndTimeChooser, marketDayAndTimeChooser, liveOnDayChooser,
      environmentRule, thetaToDayChooser, bookCloseChooser.selection, context.remotePublisher)

    def setDays(ci:CurveIdentifierLabel, tradeExpiryDay:Day) {
      // Warning - the order you set these is important - to get round this don't listen to day choosers here.
      curveDataDayAndTimeChooser.dayAndTime = ci.observationDayAndTime
      marketDayAndTimeChooser.dayAndTime = ci.forwardValuationDayAndTime
      environmentRule.rule = ci.environmentRule
      thetaToDayChooser.day = ci.thetaToDayAndTime.day
      liveOnDayChooser.day = tradeExpiryDay
    }
  }

  def updateSnapshotButton() {
    pricingGroupPanelPanel.snapshotButton.enabled = pricingGroupPanel.selection.selection.pricingGroup.isDefined
  }

  /**
   *  day 1 is the 'from' day in pnl
   */
  val day1Panel  = new MigPanel(columnConstraints = "[p][p][p]") {
    border = RoundedBorder(colour = PivotTableBackgroundColour)

    val pnlFromCheckbox = new CheckBox("Day Change:") {
      tooltip = "Day Change is calculated from this day (for curve data and market day) to the market day"
    }
    val pnlFromDayAndTimeChooser = new DayAndTimeChooser(timeOfDay0 = TimeOfDay.EndOfDay)

    val tradesAsOfLabel = new Label("Book close:") {
      tooltip = "The trades used in the report as they were at this point"
    }
    val tradesBookCloseChooser = new TimestampChooser(reportParameters.pnlParameters.flatMap(_.tradeTimestampFrom), tradeSel.desk, context)
    val useExcelButton = new ToggleToolBarButton {
      icon = StarlingIcons.icon("/icons/16x16_excel.png")
      tooltip = "If selected PnL will use excel for the 'from' market data"
    }

    add(pnlFromCheckbox)
    add(pnlFromDayAndTimeChooser.dayChooser)
    add(useExcelButton, "wrap")
    add(tradesAsOfLabel, "al right")
    add(tradesBookCloseChooser, "spanx")

    reactions += {
      case DayAndTimeChangedEvent(`curveDataDayAndTimeChooser`, d,_) => {
        if (d.day == pnlFromDayAndTimeChooser.day) {
          pnlFromDayAndTimeChooser.day = d.day.previousBusinessDay(context.localCache.ukBusinessCalendar)
        }
        updateRunButton()
      }
      case ButtonClicked(`pnlFromCheckbox`) => {
        pnlFromDayAndTimeChooser.enabled = pnlFromCheckbox.selected
        useExcelButton.enabled = pnlFromCheckbox.selected && pricingGroupPanel.selection.selection.excel.isDefined
        val tradesBookCloseEnabled = tradesBookCloseChooser.validSelection && pnlFromCheckbox.selected
        tradesBookCloseChooser.enabled = tradesBookCloseEnabled
        tradesAsOfLabel.enabled = tradesBookCloseEnabled
        updateRunButton()
      }
      case ButtonClicked(`useExcelButton`) => updateRunButton()
      case DayAndTimeChangedEvent(`pnlFromDayAndTimeChooser`,dayAndTime,_) => {
        if (dayAndTime.day == curveDataDayAndTimeChooser.day) {
          curveDataDayAndTimeChooser.day = dayAndTime.day.nextBusinessDay(context.localCache.ukBusinessCalendar)
        }
        updateRunButton()
      }
      case SelectionChanged(`tradesBookCloseChooser`) => updateRunButton()
      case MarketDataSelectionChanged(mdi) => {
        val selection = mdi.selection
        val flaggedDays = context.localCache.populatedDays(selection).toSet
        pnlFromDayAndTimeChooser.flagged = flaggedDays
        useExcelButton.enabled = pnlFromCheckbox.selected && selection.excel.isDefined
        if (!useExcelButton.enabled) {
          useExcelButton.selected = false
        }
      }
    }

    listenTo(pricingGroupPanel, curveDataDayAndTimeChooser, pnlFromDayAndTimeChooser, pnlFromCheckbox,
      tradesBookCloseChooser.selection, useExcelButton)

    def setDays(canUseExcel:Boolean, pnlParams:Option[PnlFromParameters], defaultDay:Day) {
      pnlParams match {
        case None => {
          pnlFromCheckbox.selected = false
          pnlFromDayAndTimeChooser.enabled = false
          pnlFromDayAndTimeChooser.day = defaultDay
          useExcelButton.enabled = false
          tradesBookCloseChooser.enabled = false
          tradesAsOfLabel.enabled = false
        }
        case Some(pnlP) => {
          pnlFromCheckbox.selected = true
          pnlFromDayAndTimeChooser.enabled = true
          pnlFromDayAndTimeChooser.dayAndTime = pnlP.curveIdentifierFrom.forwardValuationDayAndTime
          useExcelButton.enabled = canUseExcel
          useExcelButton.selected = pnlP.curveIdentifierFrom.marketDataIdentifier.selection.excel.isDefined
          pnlP.tradeTimestampFrom.map(tradesBookCloseChooser.selectedTimestamp = _)
          val tradesBookCloseEnabled = tradesBookCloseChooser.validSelection && pnlFromCheckbox.selected
          tradesAsOfLabel.enabled = tradesBookCloseEnabled
          tradesBookCloseChooser.enabled = tradesBookCloseEnabled
        }
      }
    }
  }

  add(day1Panel, "sgy")
  add(day2Panel, "sgy")
//  add(optionsPanel, "sgy, wrap")
  add(optionsPanel, "ay top, wrap")
  add(pricingGroupPanelPanel, "split, spanx")

  private def generateMarketDataIdentifier = {
    pricingGroupPanel.selection
  }

  def marketData(mds:MarketDataIdentifier) {
    pricingGroupPanel.selection = mds
  }

  def generateReportParams(slideConfig:SlideConfig) = {
    val mdi = generateMarketDataIdentifier
    val marketDataSelection = mdi.selection
    val pricingGroup = marketDataSelection.pricingGroup

    val envMods = TreeSet[EnvironmentModifierLabel]() ++        
        (if (!optionsPanel.discountedCheckbox.selected) Some(EnvironmentModifierLabel.zeroInterestRates) else None).toList ++
        (if (optionsPanel.zeroVolsCheckbox.selected) Some(EnvironmentModifierLabel.zeroVols) else None).toList

    val bookClose = day2Panel.bookCloseChooser.selectedTimestamp
    val tradeLiveOnDay = day2Panel.liveOnDayChooser.day
    val pnlFromDayAndTime = day1Panel.pnlFromDayAndTimeChooser.dayAndTime
    val observationDayAndTime = curveDataDayAndTimeChooser.dayAndTime
    val environmentRule = day2Panel.environmentRule.rule
    val forwardValuationDayAndTime = day2Panel.marketDayAndTimeChooser.dayAndTime
    val thetaDayAndTime: DayAndTime = day2Panel.thetaToDayChooser.day.endOfDay

    val marketDataVersion = mdi.marketDataVersion

    val pnlParams = if (day1Panel.pnlFromCheckbox.selected) {
      val fromMarketDataSelection = if (day1Panel.useExcelButton.selected) {
        marketDataSelection
      } else {
        marketDataSelection.noExcel
      }
      val rule = marketDataSelection.pricingGroup match {
        case Some(pg) if pg == PricingGroup.Metals => EnvironmentRuleLabel.MostRecentCloses
        case _ => EnvironmentRuleLabel.COB
      }

      val marketIDFrom = MarketDataIdentifier(fromMarketDataSelection, marketDataVersion)
      val cIDFrom = CurveIdentifierLabel(
        marketIDFrom,
        rule,
        pnlFromDayAndTime.day.endOfDay,
        pnlFromDayAndTime.day.endOfDay,
        pnlFromDayAndTime.nextBusinessDay(context.localCache.ukBusinessCalendar),
        envMods)
      if (tradeSel.desk.isEmpty) {
        Some(new PnlFromParameters(None, cIDFrom))
      } else {
        Some(new PnlFromParameters(Some(day1Panel.tradesBookCloseChooser.selectedTimestamp), cIDFrom))
      }
    } else {
      None
    }

    val excel = marketDataSelection.excel

    val marketDataSelectionTo = MarketDataSelection(pricingGroup, excel)
    val marketIDTo = MarketDataIdentifier(marketDataSelectionTo, marketDataVersion)

    val cIDTo = CurveIdentifierLabel(
      marketIDTo,
      environmentRule,
      observationDayAndTime,
      forwardValuationDayAndTime,
      thetaDayAndTime,
      envMods)

    val deskWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map{ case (d, _) => (d, bookClose) }
    val intradayWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map{
      case (i, _) => (i, context.localCache.latestTimestamp(i))
    }

    val newReportOptions = reportParameters.reportOptions.copy(slide1 = slideConfig.slide1, slide2 = slideConfig.slide2)

    ReportParameters(
      tradeSelectionWithTimestamp = TradeSelectionWithTimestamp(deskWithNewTimestamp, reportParameters.tradeSelectionWithTimestamp.tradePredicate, intradayWithNewTimestamp),
      curveIdentifier = cIDTo,
      reportOptions = newReportOptions,
      expiryDay = tradeLiveOnDay,
      pnlParameters = pnlParams,
      runReports = true
    )    
  }

  def updateRunButton() {
    publish(UpdateRunButtonEvent(this))
  }

  reactions += {
    case _ : MarketDataSnapshot => updateRunButton()
  }
  listenTo(context.remotePublisher)

  def setupState(rp:ReportParameters) {
    pricingGroupPanel.selection = rp.curveIdentifier.marketDataIdentifier

    day2Panel.setDays(
      rp.curveIdentifier,
      rp.expiryDay)

    val canUseExcel = rp.curveIdentifier.marketDataIdentifier.selection.excel.isDefined
    day1Panel.setDays(canUseExcel, rp.pnlParameters, rp.curveIdentifier.observationDayAndTime.day.previousBusinessDay(context.localCache.ukBusinessCalendar))

    val envMods = rp.curveIdentifier.envModifiers
    optionsPanel.discountedCheckbox.selected = !envMods.contains(EnvironmentModifierLabel.zeroInterestRates)
    optionsPanel.zeroVolsCheckbox.selected = envMods.contains(EnvironmentModifierLabel.zeroVols)

    rp.tradeSelectionWithTimestamp.deskAndTimestamp match {
      case Some((_, ts)) => day2Panel.bookCloseChooser.selectedTimestamp = ts
      case None => {
        day2Panel.bookCloseChooser.enabled = false
        day1Panel.tradesBookCloseChooser.enabled = false
      }
    }

    updateSnapshotButton()
  }

  override def revert() {
    setupState(reportParameters)
    updateRunButton()
  }

  setupState(reportParameters)
}

case class UpdateRunButtonEvent(source:Component) extends Event
case class MarketDataChanged(source:Component) extends Event
