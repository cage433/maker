package starling.gui

import api._
import pages._
import swing._
import event._
import collection.immutable.TreeSet
import starling.daterange._
import starling.gui.StarlingLocalCache._
import starling.browser.PageContext
import starling.browser.common.{RoundedBorder, MigPanel}
import starling.browser.common.GuiUtils._

class ManualReportConfigPanel(context:PageContext, reportParameters:ReportParameters, pivotPageState:PivotPageState)
        extends MigPanel with ConfigPanel {
  def displayName = "Manual"

  val tradeSel = reportParameters.tradeSelectionWithTimestamp.asTradeSelection
  val initialTradesAsOf = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map(_._2)

  val pricingGroups = context.localCache.pricingGroups(tradeSel.desk)
  val pricingGroupPanel = new MarketDataSelectionComponent(
    context,
    reportParameters.tradeSelectionWithTimestamp.desk,
    reportParameters.curveIdentifier.marketDataIdentifier.selection) {
    reactions += { case MarketDataSelectionChanged(selection) => {
      ManualReportConfigPanel.this.publish(MarketDataChanged(ManualReportConfigPanel.this))
      updateSnapshotButton
    } }
  }
  val pricingGroupPanelPanel = new MigPanel {
    border = RoundedBorder(colour = PivotTableBackgroundColour)
    add(pricingGroupPanel, "push, grow")
  }

  val optionsPanel = new MigPanel(columnConstraints = "[p]unrel[p]", rowConstraints = "push[p]push") {
    border = RoundedBorder(PivotTableBackgroundColour)
    val zeroInterestRatesCheckbox = new CheckBox("Zero Interest Rates") {
      tooltip = "Select this to run reports without applying discounting"
    }
    val zeroVolsCheckbox = new CheckBox("Zero Vols") {
      tooltip = "Select this to run reports with all volatilities set to 0%"
    }

    add(zeroInterestRatesCheckbox, zeroVolsCheckbox)

    reactions += {
      case ButtonClicked(_) => updateRunButton
    }
    listenTo(zeroInterestRatesCheckbox, zeroVolsCheckbox)
  }

  val observationDayChooser = new DayChooser()

   /**
   * The panel for all the day 2 valuation fields. In the context where pnl is not chosen, d2 is the only active valuation information.
   */
  val day2Panel = new MigPanel(columnConstraints = "[p][p][p]unrel[p][p]unrel[p][p]") {
    border = RoundedBorder(PivotTableBackgroundColour)

    val environmentRuleLabel = new Label("Env Rule:") {
      tooltip = "The rule for selecting and deriving curves from market data"
    }
    val snapshotButton = new Button {
      tooltip = "Import and snapshot market data for the day selected"
      icon = StarlingIcons.icon("/icons/14x14_download_data.png")
      reactions += {
        case ButtonClicked(_) => {
          context.submit(SnapshotSubmitRequest(generateMarketDataSelection, observationDayChooser.day))
        }
      }
    }
    val environmentRule = new EnvironmentRuleChooser(
      reportParameters.curveIdentifier.environmentRule,
      context.localCache.environmentRulesForPricingGroup(reportParameters.curveIdentifier.marketDataIdentifier.selection.pricingGroup)
    )

    val forwardObservationDayLabel = new Label("Forward Observation:") {
      tooltip = "The day used for time to expiry and discounting (usually the same as the observation day)"
    }
    val forwardObservationDayAndTimeChooser = new DayAndTimeChooser(timeOfDay0 = TimeOfDay.StartOfDay)

    val thetaToLabel = new Label("Theta to:") {
      tooltip = "Theta is calculated as the change between the forward observation day and this day (usually the business day after the observation day)"
    }
    val thetaToDayChooser = new DayChooser()

    val observationDayLabel = new Label("<html><b>Observation day:</b></html>") {
      tooltip = "Trades with a trade day after this day will be ignored"
    }

    val liveOnLabel = new Label("Live on:") {
      tooltip = "Trades that expire before this day are excluded"
    }
    val liveOnDayChooser = new DayChooser(enableFlags = false)
    
    val bookCloseLabel = new Label("Book close:") {
      tooltip = "The trades used in the report as they were at this point"
    }
    val bookCloseChooser = new TimestampChooser(initialTradesAsOf, tradeSel.desk, context)

    add(observationDayLabel, observationDayChooser, snapshotButton)
    add(environmentRuleLabel)
    add(environmentRule)
    add(thetaToLabel)
    add(thetaToDayChooser, "wrap")

    add(forwardObservationDayLabel, forwardObservationDayAndTimeChooser, forwardObservationDayAndTimeChooser.timeOfDayChooser)
    add(bookCloseLabel)
    add(bookCloseChooser)
    add(liveOnLabel)
    add(liveOnDayChooser)

    def updatePopulatedDays(selection:MarketDataSelection=pricingGroupPanel.selection) {
      val flaggedDays = context.localCache.populatedDays(selection).toSet
      observationDayChooser.flagged = flaggedDays
      forwardObservationDayAndTimeChooser.flagged = flaggedDays
      thetaToDayChooser.flagged = flaggedDays
    }

    reactions += {
      case DayChangedEvent(`observationDayChooser`, d) => {
        val timeOfDayToUse = if (d >= Day.today) TimeOfDay.StartOfDay else TimeOfDay.EndOfDay
        forwardObservationDayAndTimeChooser.dayAndTime = d.atTimeOfDay(timeOfDayToUse)
        liveOnDayChooser.day = d
      }
      case DayAndTimeChangedEvent(`forwardObservationDayAndTimeChooser`, dayAndTime) => {
        thetaToDayChooser.day = dayAndTime.day.nextBusinessDay(context.localCache.ukBusinessCalendar)
      }
      case EnvironmentRuleLabelChangedEvent(_, _) => updateRunButton
      case ExcelObservationDay(_, _) | PricingGroupObservationDay(_, _) => updatePopulatedDays()
      case DayChangedEvent(_,_) => updateRunButton
      case SelectionChanged(`bookCloseChooser`) => updateRunButton
      case MarketDataSelectionChanged(selection) => {
        val valuationDay = observationDayChooser.day
        updatePopulatedDays(selection)
      }
    }

    listenTo(pricingGroupPanel, observationDayChooser, forwardObservationDayAndTimeChooser, liveOnDayChooser,
      environmentRule, thetaToDayChooser, bookCloseChooser.selection, context.remotePublisher)

    def setDays(ci:CurveIdentifierLabel, tradeExpiryDay:Day) {
      // Warning - the order you set these is important - to get round this don't listen to day choosers here.
      observationDayChooser.day = ci.tradesUpToDay
      forwardObservationDayAndTimeChooser.dayAndTime = ci.valuationDayAndTime
      environmentRule.rule = ci.environmentRule
      thetaToDayChooser.day = ci.thetaDayAndTime.day
      liveOnDayChooser.day = tradeExpiryDay
    }
  }

  def updateSnapshotButton {
    day2Panel.snapshotButton.enabled = pricingGroupPanel.selection.pricingGroup.isDefined
  }


  /**
   *  day 1 is the 'from' day in pnl
   */
  val day1Panel  = new MigPanel(columnConstraints = "[p][p][p]") {
    border = RoundedBorder(colour = PivotTableBackgroundColour)

    val pnlFromCheckbox = new CheckBox("Day Change:") {
      tooltip = "Day Change is calculated from this day (market data and valuation) to the observation day"
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
    add(pnlFromDayAndTimeChooser.dayChooser, "sgx")
    add(pnlFromDayAndTimeChooser.timeOfDayChooser, "wrap")
    add(tradesAsOfLabel)
    add(tradesBookCloseChooser, "sgx")
    add(useExcelButton)

    reactions += {
      case DayChangedEvent(`observationDayChooser`, d) => {
        pnlFromDayAndTimeChooser.day = d.previousBusinessDay(context.localCache.ukBusinessCalendar)
      }
      case ButtonClicked(`pnlFromCheckbox`) => {
        pnlFromDayAndTimeChooser.enabled = pnlFromCheckbox.selected
        useExcelButton.enabled = pnlFromCheckbox.selected && pricingGroupPanel.selection.excel.isDefined
        val tradesBookCloseEnabled = tradesBookCloseChooser.validSelection && pnlFromCheckbox.selected
        tradesBookCloseChooser.enabled = tradesBookCloseEnabled
        tradesAsOfLabel.enabled = tradesBookCloseEnabled
        updateRunButton
      }
      case ButtonClicked(`useExcelButton`) => updateRunButton
      case DayAndTimeChangedEvent(_,_) => updateRunButton
      case SelectionChanged(`tradesBookCloseChooser`) => updateRunButton
      case MarketDataSelectionChanged(selection) => {
        val flaggedDays = context.localCache.populatedDays(selection).toSet
        pnlFromDayAndTimeChooser.flagged = flaggedDays
        useExcelButton.enabled = pnlFromCheckbox.selected && selection.excel.isDefined
        if (!useExcelButton.enabled) {
          useExcelButton.selected = false
        }
      }
    }

    listenTo(pricingGroupPanel, observationDayChooser, pnlFromDayAndTimeChooser, pnlFromCheckbox,
      tradesBookCloseChooser.selection, useExcelButton)

    def setDays(canUseExcel:Boolean, pnlParams:Option[PnlFromParameters]) {
      pnlParams match {
        case None => {
          pnlFromCheckbox.selected = false
          pnlFromDayAndTimeChooser.enabled = false
          useExcelButton.enabled = false
          tradesBookCloseChooser.enabled = false
          tradesAsOfLabel.enabled = false
        }
        case Some(pnlP) => {
          pnlFromCheckbox.selected = true
          pnlFromDayAndTimeChooser.enabled = true
          pnlFromDayAndTimeChooser.dayAndTime = pnlP.curveIdentifierFrom.valuationDayAndTime
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

  add(day1Panel, "sgy 2")
  add(day2Panel, "sgy 2, split, spanx, wrap")
  add(pricingGroupPanelPanel, "split, spanx, sgy")
  add(optionsPanel, "sgy")

  private def generateMarketDataSelection = {
    pricingGroupPanel.selection
  }

  def marketData(mds:MarketDataSelection) {
    pricingGroupPanel.selection = mds
  }

  def generateReportParams(slideConfig:SlideConfig) = {
    val marketDataSelection = generateMarketDataSelection
    val pricingGroup = marketDataSelection.pricingGroup

    val envMods = TreeSet[EnvironmentModifierLabel]() ++        
        (if (optionsPanel.zeroInterestRatesCheckbox.selected) Some(EnvironmentModifierLabel.zeroInterestRates) else None).toList ++
        (if (optionsPanel.zeroVolsCheckbox.selected) Some(EnvironmentModifierLabel.zeroVols) else None).toList

    val bookClose = day2Panel.bookCloseChooser.selectedTimestamp
    val tradeLiveOnDay = day2Panel.liveOnDayChooser.day
    val pnlFromDayAndTime = day1Panel.pnlFromDayAndTimeChooser.dayAndTime
    val valuationDay = observationDayChooser.day
    val environmentRule = day2Panel.environmentRule.rule
    val forwardValuationDayAndTime = day2Panel.forwardObservationDayAndTimeChooser.dayAndTime
    val thetaDayAndTime: DayAndTime = day2Panel.thetaToDayChooser.day.endOfDay()

    val marketDataVersion = context.localCache.latestMarketDataVersion(marketDataSelection)

    val pnlParams = if (day1Panel.pnlFromCheckbox.selected) {
      val fromMarketDataSelection = if (day1Panel.useExcelButton.selected) {
        marketDataSelection
      } else {
        marketDataSelection.noExcel
      }
      val rule = if(day1Panel.useExcelButton.selected) {
        EnvironmentRuleLabel.RealTime
      } else {
        EnvironmentRuleLabel.COB
      }

      val marketIDFrom = MarketDataIdentifier(fromMarketDataSelection, marketDataVersion)
      val cIDFrom = CurveIdentifierLabel(
        marketIDFrom,
        rule,
        pnlFromDayAndTime.day,
        pnlFromDayAndTime,
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
      valuationDay,
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

  def updateRunButton {
    publish(UpdateRunButtonEvent(this))
  }

  reactions += {
    case MarketDataSnapshot(_) => updateRunButton
  }
  listenTo(context.remotePublisher)

  def setupState(rp:ReportParameters) {
    pricingGroupPanel.selection = rp.curveIdentifier.marketDataIdentifier.selection

    day2Panel.setDays(
      rp.curveIdentifier,
      rp.expiryDay)

    val canUseExcel = rp.curveIdentifier.marketDataIdentifier.selection.excel.isDefined
    day1Panel.setDays(canUseExcel, rp.pnlParameters)

    val envMods = rp.curveIdentifier.envModifiers
    optionsPanel.zeroInterestRatesCheckbox.selected = envMods.contains(EnvironmentModifierLabel.zeroInterestRates)
    optionsPanel.zeroVolsCheckbox.selected = envMods.contains(EnvironmentModifierLabel.zeroVols)

    rp.tradeSelectionWithTimestamp.deskAndTimestamp match {
      case Some((_, ts)) => day2Panel.bookCloseChooser.selectedTimestamp = ts
      case None => {
        day2Panel.bookCloseChooser.enabled = false
        day1Panel.tradesBookCloseChooser.enabled = false
      }
    }

    updateSnapshotButton
  }

  override def revert() {
    setupState(reportParameters)
    updateRunButton
  }

  setupState(reportParameters)
}

case class UpdateRunButtonEvent(source:Component) extends Event
case class MarketDataChanged(source:Component) extends Event
