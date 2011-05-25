package starling.gui

import api._
import api.MarketDataIdentifier._
import pages._
import pages.MarketDataSelectionChanged._
import starling.pivot.view.swing.MigPanel
import starling.gui.GuiUtils._
import swing._
import event.SelectionChanged._
import event.{SelectionChanged, ButtonClicked}
import swing.Orientation._
import collection.immutable.TreeSet
import starling.daterange.Day
import collection.mutable.ListBuffer
import collection.mutable.ListBuffer._

object ReportPreset extends Enumeration {
  type ReportPreset = Value
  val RealTime, COB, Manual = Value
}

import ReportPreset._

class PresetReportConfigPanel(context:PageContext, reportParameters:ReportParameters, pivotPageState:PivotPageState)
        extends MigPanel(columnConstraints = "[p]push[p]") with ConfigPanel {
  def displayName = "Presets"

  val pricingGroupPanel = new MigPanel {
    border = RoundedBorder(colour = PivotTableBackgroundColour)

    val pricingGroupSelector = new MarketDataSelectionComponent(
      context,
      reportParameters.tradeSelectionWithTimestamp.desk,
      reportParameters.curveIdentifier.marketDataIdentifier.selection, scala.swing.Orientation.Vertical) {
      reactions += { case MarketDataSelectionChanged(selection) => {
        PresetReportConfigPanel.this.publish(MarketDataChanged(PresetReportConfigPanel.this))
      } }
    }

    add(pricingGroupSelector, "push, grow")
  }

  val buttonPanel = new MigPanel(columnConstraints = "[p]unrel[p]unrel[p]") {
    border = RoundedBorder(colour = PivotTableBackgroundColour)
    val realTimeButton = new RadioButton("Real Time")
    val cobButton = new RadioButton("COB")
    val manualButton = new RadioButton("Manual")
    val group = new ButtonGroup(realTimeButton, cobButton, manualButton)
    group.select(realTimeButton)

    private val buttonToPreset = Map[AbstractButton, ReportPreset](realTimeButton -> RealTime, cobButton -> COB, manualButton -> Manual)
    private val presetToButton = buttonToPreset.map{case (k,v) => (v->k)}
    def preset = buttonToPreset(group.selected.get)
    def preset_=(p:ReportPreset) {
      group.select(presetToButton(p))
      contentPanel.update(buttonToPanel(group.selected.get))
    }

    val sep = new Separator(Vertical)

    val contentPanel = new MigPanel("insets 0") {
      def update(comp:Component) {
        removeAll
        add(comp, "push,grow")
        revalidate()
        repaint()
      }
    }

    val realTimePanel = new MigPanel("insets 0") {
      val dayChangeCheckBox = new CheckBox("Day Change") {
        tooltip = "Include day change PnL"
      }
      add(dayChangeCheckBox)

      reactions += {
        case ButtonClicked(`dayChangeCheckBox`) => updateRunButton()
      }
      listenTo(dayChangeCheckBox)
    }

    val observationDayChooser = new DayChooser(Day.today().previousBusinessDay(context.localCache.ukBusinessCalendar))

    val cobPanel = new MigPanel("insets 0", "[p]unrel[p]unrel[p]") {
      val dayChangePanel = new MigPanel("insets 0, hidemode 3") {
        val dayChangeCheckBox = new CheckBox("Day Change") {
          tooltip = "Day Change is calculated from this day (market data and valuation) to the observation day"
        }
        val dayChangeDayChooser = new DayChooser(observationDayChooser.day.previousBusinessDay(context.localCache.ukBusinessCalendar))
        val bookCloseLabel = new Label("Book close:") {
          tooltip = "The trades used in the report as they were at this point"
        }
        val tradeSel = reportParameters.tradeSelectionWithTimestamp.asTradeSelection
        val bookCloseChooser = new TimestampChooser(reportParameters.pnlParameters.flatMap(_.tradeTimestampFrom), tradeSel.desk, context)
        val useExcelButton = new ToggleToolBarButton {
          icon = StarlingIcons.icon("/icons/16x16_excel.png")
          tooltip = "If selected day change PnL will use excel for the 'from' market data day"
        }
        dayChangeCheckBox.preferredSize = new Dimension(dayChangeCheckBox.preferredSize.width, useExcelButton.preferredSize.height)
        realTimePanel.dayChangeCheckBox.preferredSize = new Dimension(realTimePanel.dayChangeCheckBox.preferredSize.width, useExcelButton.preferredSize.height)

        add(dayChangeCheckBox)
        add(dayChangeDayChooser, "sgx")
        add(useExcelButton, "wrap")
        add(bookCloseLabel, "al right")
        add(bookCloseChooser, "sgx")

        def setDays(canUseExcel:Boolean, pnlParams:Option[PnlFromParameters]) {
          pnlParams match {
            case None => {
              dayChangeCheckBox.selected = false
              dayChangeDayChooser.enabled = false
              useExcelButton.enabled = false
              bookCloseChooser.enabled = false
              bookCloseLabel.enabled = false
              displayComponents(false)
            }
            case Some(pnlP) => {
              dayChangeCheckBox.selected = true
              dayChangeDayChooser.enabled = true
              dayChangeDayChooser.day = pnlP.curveIdentifierFrom.valuationDayAndTime.day
              useExcelButton.enabled = canUseExcel
              useExcelButton.selected = pnlP.curveIdentifierFrom.marketDataIdentifier.selection.excel.isDefined
              pnlP.tradeTimestampFrom.map(bookCloseChooser.selectedTimestamp = _)
              val bookCloseEnabled = bookCloseChooser.validSelection && dayChangeCheckBox.selected
              bookCloseChooser.enabled = bookCloseEnabled
              bookCloseLabel.enabled = bookCloseEnabled
              displayComponents(true)
            }
          }
        }

        def displayComponents(display:Boolean) {
          dayChangeDayChooser.visible = display
          bookCloseLabel.visible = display
          bookCloseLabel.visible = display
          bookCloseChooser.visible = display
          useExcelButton.visible = display
          if (display) {
            dayChangeCheckBox.text = "Day Change:"
          } else {
            dayChangeCheckBox.text = "Day Change"
          }
        }

        reactions += {
          case DayChangedEvent(`observationDayChooser`, d) => {
            dayChangeDayChooser.day = d.previousBusinessDay(context.localCache.ukBusinessCalendar)
          }
          case DayChangedEvent(`dayChangeDayChooser`, d) => updateRunButton()
          case ButtonClicked(`dayChangeCheckBox`) => {
            displayComponents(dayChangeCheckBox.selected)
            dayChangeDayChooser.enabled = dayChangeCheckBox.selected
            useExcelButton.enabled = dayChangeCheckBox.selected && pricingGroupPanel.pricingGroupSelector.selection.excel.isDefined
            val bookClosedEnabled = bookCloseChooser.validSelection && dayChangeCheckBox.selected
            bookCloseChooser.enabled = bookClosedEnabled
            bookCloseLabel.enabled = bookClosedEnabled
            updateRunButton()
          }
          case ButtonClicked(`useExcelButton`) => updateRunButton()
          case SelectionChanged(`bookCloseChooser`) => updateRunButton()
          case MarketDataSelectionChanged(selection) => {
            val flaggedDays = context.localCache.populatedDays(selection).toSet
            dayChangeDayChooser.flagged = flaggedDays

            useExcelButton.enabled = dayChangeCheckBox.selected && selection.excel.isDefined
            if (!useExcelButton.enabled) {
              useExcelButton.selected = false
            }
          }
        }

        listenTo(pricingGroupPanel.pricingGroupSelector, observationDayChooser, dayChangeDayChooser, dayChangeCheckBox,
          bookCloseChooser.selection, useExcelButton)

        displayComponents(false)
      }

      val dayPanel = new MigPanel("insets 0") {
        val observationDayLabel = new Label("<html><b>Observation day:</b></html>") {
          tooltip = "Trades with a trade day after this day will be ignored"
        }
        val liveTradesCheckBox = new CheckBox("Live Trades Only") {
          tooltip = "No trades that have expired will be included"
        }

        add(observationDayLabel)
        add(observationDayChooser, "wrap")
        add(liveTradesCheckBox, "split, spanx")

        def setDays(ci:CurveIdentifierLabel, tradeExpiryDay:Day) {
          observationDayChooser.day = ci.tradesUpToDay
          liveTradesCheckBox.selected = (ci.tradesUpToDay == tradeExpiryDay)
        }

        reactions += {
          case DayChangedEvent(`observationDayChooser`, d) => updateRunButton()
          case MarketDataSelectionChanged(selection) => {
            val flaggedDays = context.localCache.populatedDays(selection).toSet
            observationDayChooser.flagged = flaggedDays
          }
          case ExcelObservationDay(_, _) | PricingGroupObservationDay(_, _) => {
            val flaggedDays = context.localCache.populatedDays(pricingGroupPanel.pricingGroupSelector.selection).toSet
            observationDayChooser.flagged = flaggedDays
          }
          case ButtonClicked(`liveTradesCheckBox`) => updateRunButton()
        }
        listenTo(observationDayChooser, pricingGroupPanel.pricingGroupSelector, context.remotePublisher, liveTradesCheckBox)
      }
      val sep = new Separator(Vertical)

      add(dayChangePanel, "ay top, sgy")
      add(sep, "pushy, growy")
      add(dayPanel, "ay top, sgy")
    }

    val manualPanel = new MigPanel("insets 0") {
      add(new Label("Please configure your report using the Manual tab "), "ay top")
    }

    val buttonToPanel = Map[AbstractButton, Component](realTimeButton -> realTimePanel, cobButton -> cobPanel, manualButton -> manualPanel)

    contentPanel.update(realTimePanel)

    add(realTimeButton, "split 3, flowy, gapbottom 0")
    add(cobButton, "gapbottom 0")
    add(manualButton)
    add(sep, "growy")
    add(contentPanel, "ay top, push, grow")

    reactions += {
      case ButtonClicked(_) => {
        contentPanel.update(buttonToPanel(group.selected.get))
        updateRunButton()
      }
    }
    listenTo(realTimeButton, cobButton, manualButton)
  }

  private def generateMarketDataSelection = {
    pricingGroupPanel.pricingGroupSelector.selection
  }

  add(buttonPanel, "sgy")
  add(pricingGroupPanel, "sgy")

  private def isRealTime(rp:ReportParameters) = {
    val realTime = new ListBuffer[Boolean]()

    val today = Day.today()
    val nextBusinessDay = today.nextBusinessDay(context.localCache.ukBusinessCalendar)
    val previousBusinessDay = today.previousBusinessDay(context.localCache.ukBusinessCalendar)

    realTime += (rp.curveIdentifier.environmentRule == EnvironmentRuleLabel.RealTime)
    realTime += (rp.curveIdentifier.tradesUpToDay == today)
    realTime += (rp.curveIdentifier.valuationDayAndTime == today.startOfDay())
    realTime += (rp.curveIdentifier.thetaDayAndTime == nextBusinessDay.endOfDay())

    realTime += (rp.expiryDay == today)

    val desk = rp.tradeSelectionWithTimestamp.desk
    val allBookCloses = context.localCache.deskCloses(desk) match {
      case Nil =>  List(TimestampChooser.defaultUnitialisedValue)
      case l => l
    }
    val bookCloseShouldBe = allBookCloses.head
    rp.tradeSelectionWithTimestamp.deskAndTimestamp match {
      case None =>
      case Some((desk, timestamp)) => {
        realTime += (bookCloseShouldBe.closeDay == timestamp.closeDay)
      }
    }

    rp.pnlParameters match {
      case None =>
      case Some(pnl) => {
        realTime += (pnl.curveIdentifierFrom.tradesUpToDay == previousBusinessDay)
        pnl.tradeTimestampFrom match {
          case None =>
          case Some(tts) => {
            realTime += (bookCloseShouldBe.closeDay == tts.closeDay)
          }
        }
      }
    }

    realTime.toList.forall(_ == true)
  }

  private def isCOB(rp:ReportParameters) = {
    val cob = new ListBuffer[Boolean]()

    val observationDayUsed = rp.curveIdentifier.tradesUpToDay
    val previousBusinessDay = observationDayUsed.previousBusinessDay(context.localCache.ukBusinessCalendar)
    val nextBusinessDay = observationDayUsed.nextBusinessDay(context.localCache.ukBusinessCalendar)

    cob += ((rp.expiryDay == observationDayUsed) || (rp.expiryDay == observationDayUsed.startOfFinancialYear))
    cob += (rp.curveIdentifier.environmentRule == EnvironmentRuleLabel.COB)
    cob += (rp.curveIdentifier.valuationDayAndTime == observationDayUsed.endOfDay())
    cob += (rp.curveIdentifier.thetaDayAndTime == nextBusinessDay.endOfDay())

    val desk = rp.tradeSelectionWithTimestamp.desk
    val allBookCloses = context.localCache.deskCloses(desk) match {
      case Nil =>  List(TimestampChooser.defaultUnitialisedValue)
      case l => l
    }
    val bookCloseShouldBe = allBookCloses.head
    rp.tradeSelectionWithTimestamp.deskAndTimestamp match {
      case None =>
      case Some((desk, timestamp)) => {
        cob += (bookCloseShouldBe.closeDay == timestamp.closeDay)
      }
    }

    cob.toList.forall(_ == true)
  }

  def setupState(rp:ReportParameters) {
    pricingGroupPanel.pricingGroupSelector.revert

    if (isRealTime(rp)) {
      buttonPanel.group.select(buttonPanel.realTimeButton)
      buttonPanel.contentPanel.update(buttonPanel.realTimePanel)
      buttonPanel.realTimePanel.dayChangeCheckBox.selected = rp.pnlParameters.isDefined
    } else if (isCOB(rp)) {
      buttonPanel.group.select(buttonPanel.cobButton)
      buttonPanel.contentPanel.update(buttonPanel.cobPanel)
      val canUseExcel = rp.curveIdentifier.marketDataIdentifier.selection.excel.isDefined
      buttonPanel.cobPanel.dayChangePanel.setDays(canUseExcel, rp.pnlParameters)
      buttonPanel.cobPanel.dayPanel.setDays(rp.curveIdentifier, rp.expiryDay)
    } else {
      buttonPanel.group.select(buttonPanel.manualButton)
      buttonPanel.contentPanel.update(buttonPanel.manualPanel)
    }
  }

  override def revert() {setupState(reportParameters)}

  private def updateRunButton() {
    publish(UpdateRunButtonEvent(this))
  }

  def marketData(mds:MarketDataSelection) {
    pricingGroupPanel.pricingGroupSelector.selection = mds
  }

  def generateRealTimeReportParameters = {
    val today = Day.today()
    
    val desk = reportParameters.tradeSelectionWithTimestamp.desk
    val allBookCloses = context.localCache.deskCloses(desk) match {
      case Nil =>  List(TimestampChooser.defaultUnitialisedValue)
      case l => l
    }
    val toBookClose = allBookCloses.head

    val deskWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map{ case (d, _) => (d, toBookClose) }
    val intradayWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map{
      case (i, _) => (i, context.localCache.latestTimestamp(i))
    }

    val marketDataSelection = generateMarketDataSelection
    val marketDataVersion = SpecificMarketDataVersion(context.localCache.latestMarketDataVersion(marketDataSelection))
    val pricingGroup = marketDataSelection.pricingGroup

    val excel = marketDataSelection.excel
    val marketDataSelectionTo = MarketDataSelection(pricingGroup, excel)
    val marketIDTo = MarketDataIdentifier(marketDataSelectionTo, marketDataVersion)

    val previousBusinessDay = today.previousBusinessDay(context.localCache.ukBusinessCalendar)
    val nextBusinessDay = today.nextBusinessDay(context.localCache.ukBusinessCalendar)

    val envMods = TreeSet[EnvironmentModifierLabel](EnvironmentModifierLabel.zeroInterestRates)

    val cIDTo = CurveIdentifierLabel(
      marketIDTo,
      EnvironmentRuleLabel.RealTime,
      today,
      today.startOfDay(),
      nextBusinessDay.endOfDay(),
      envMods)

    val newReportOptions = ReportOptions.Blank

    val pnlParams = if (buttonPanel.realTimePanel.dayChangeCheckBox.selected) {
      val fromMarketDataSelection = marketDataSelection.noExcel
      val marketIDFrom = MarketDataIdentifier(fromMarketDataSelection, marketDataVersion)
      val pnlFromDayAndTime = previousBusinessDay.endOfDay()
      val cIDFrom = CurveIdentifierLabel(
        marketIDFrom,
        EnvironmentRuleLabel.COB,
        pnlFromDayAndTime.day,
        pnlFromDayAndTime,
        pnlFromDayAndTime.nextBusinessDay(context.localCache.ukBusinessCalendar),
        envMods)
      if (desk.isEmpty) {
        Some(new PnlFromParameters(None, cIDFrom))
      } else {
        Some(new PnlFromParameters(Some(toBookClose), cIDFrom))
      }
    } else {
      None
    }

    ReportParameters(
      tradeSelectionWithTimestamp = TradeSelectionWithTimestamp(deskWithNewTimestamp, reportParameters.tradeSelectionWithTimestamp.tradePredicate, intradayWithNewTimestamp),
      curveIdentifier = cIDTo,
      reportOptions = newReportOptions,
      expiryDay = today,
      pnlParameters = pnlParams,
      runReports = true
    )
  }

  def generateCOBReportParameters = {
    val observationDay = buttonPanel.observationDayChooser.day
    val tradeLiveOnDay = if (buttonPanel.cobPanel.dayPanel.liveTradesCheckBox.selected) {
      observationDay
    } else {
      observationDay.startOfFinancialYear
    }

    val desk = reportParameters.tradeSelectionWithTimestamp.desk
    val allBookCloses = context.localCache.deskCloses(desk) match {
      case Nil =>  List(TimestampChooser.defaultUnitialisedValue)
      case l => l
    }
    val toBookClose = allBookCloses.head

    val deskWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map{ case (d, _) => (d, toBookClose) }
    val intradayWithNewTimestamp = reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map{
      case (i, _) => (i, context.localCache.latestTimestamp(i))
    }

    val marketIDTo = generateMarketDataIdentifier

    val nextBusinessDay = observationDay.nextBusinessDay(context.localCache.ukBusinessCalendar)

    val envMods = TreeSet[EnvironmentModifierLabel](EnvironmentModifierLabel.zeroInterestRates)

    val cIDTo = CurveIdentifierLabel(
      marketIDTo,
      EnvironmentRuleLabel.COB,
      observationDay,
      observationDay.endOfDay(),
      nextBusinessDay.endOfDay(),
      envMods)

    val newReportOptions = ReportOptions.Blank

    val pnlParams = if (buttonPanel.cobPanel.dayChangePanel.dayChangeCheckBox.selected) {
      val marketDataSelection = generateMarketDataSelection
      val marketDataVersion = SpecificMarketDataVersion(context.localCache.latestMarketDataVersion(marketDataSelection))
      val fromMarketDataSelection = if (buttonPanel.cobPanel.dayChangePanel.useExcelButton.selected) {
        marketDataSelection
      } else {
        marketDataSelection.noExcel
      }
      val marketIDFrom = MarketDataIdentifier(fromMarketDataSelection, marketDataVersion)
      val pnlFromDayAndTime = buttonPanel.cobPanel.dayChangePanel.dayChangeDayChooser.day.endOfDay()
      val cIDFrom = CurveIdentifierLabel(
        marketIDFrom,
        EnvironmentRuleLabel.COB,
        pnlFromDayAndTime.day,
        pnlFromDayAndTime,
        pnlFromDayAndTime.nextBusinessDay(context.localCache.ukBusinessCalendar),
        envMods)
      if (desk.isEmpty) {
        Some(new PnlFromParameters(None, cIDFrom))
      } else {
        Some(new PnlFromParameters(Some(buttonPanel.cobPanel.dayChangePanel.bookCloseChooser.selectedTimestamp), cIDFrom))
      }
    } else {
      None
    }

    ReportParameters(
      tradeSelectionWithTimestamp = TradeSelectionWithTimestamp(deskWithNewTimestamp, reportParameters.tradeSelectionWithTimestamp.tradePredicate, intradayWithNewTimestamp),
      curveIdentifier = cIDTo,
      reportOptions = newReportOptions,
      expiryDay = tradeLiveOnDay,
      pnlParameters = pnlParams,
      runReports = true
    )
  }

  def generateReportParameters = {
    buttonPanel.preset match {
      case RealTime => Some(generateRealTimeReportParameters)
      case COB => Some(generateCOBReportParameters)
      case _ => None
    }
  }

  def generateMarketDataIdentifier = {
    val marketDataSelection = generateMarketDataSelection
    val marketDataVersion = SpecificMarketDataVersion(context.localCache.latestMarketDataVersion(marketDataSelection))
    val pricingGroup = marketDataSelection.pricingGroup

    val excel = marketDataSelection.excel
    val marketDataSelectionTo = MarketDataSelection(pricingGroup, excel)
    MarketDataIdentifier(marketDataSelectionTo, marketDataVersion)
  }

  revert
}