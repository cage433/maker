package starling.gui.pages

import java.awt.Dimension
import starling.gui._
import api._
import starling.pivot.PivotFieldParams
import starling.daterange.Day
import swing.Swing._
import starling.browser.common.GuiUtils._
import swing._
import collection.mutable.ListBuffer
import event.{SelectionChanged, ButtonClicked, MouseClicked}
import javax.swing.DefaultComboBoxModel
import starling.tradestore.TradePredicate
import utils.RichReactor
import RichReactor._
import starling.gui.StarlingLocalCache._
import starling.browser._
import common.{ButtonClickedEx, NewPageButton, MigPanel}

case class ReportConfigurationPage(tradeAndReferenceDataInfo:TradeAndReferenceDataInfo) extends StarlingServerPage {
  def text = "Configure Report"

  def icon = StarlingIcons.im("/icons/16x16_report.png")
  // This page doesn't need anything from the server.
  def build(reader:StarlingServerContext) = {null}

  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = {
    new ReportConfigurationComponent(context, tradeAndReferenceDataInfo)
  }
}

case class TradeAndReferenceDataInfo(tradePredicate: TradePredicate, deskAndTimestamp:Option[(Desk, TradeTimestamp)],
                                     intradayGroups: Option[IntradayGroups], tradeExpiryDay:Day) {
  def tradeSelection = new TradeSelection(deskAndTimestamp.map(_._1), tradePredicate, intradayGroups)
}

class ReportConfigurationComponent(context:PageContext, tradeAndReferenceDataInfo:TradeAndReferenceDataInfo) extends BorderPanel with PageComponent {
  val tradeSelection = tradeAndReferenceDataInfo.tradeSelection
  val tradeExpiryDay = tradeAndReferenceDataInfo.tradeExpiryDay
  val desk = tradeAndReferenceDataInfo.deskAndTimestamp.map(_._1)
  val tradeTimestamp = tradeAndReferenceDataInfo.deskAndTimestamp.map(_._2)

  def generateTradeDetailsComponent = new MigPanel("insets 0", "[" + StandardLeftIndent + "][fill,grow]", "[fill,grow]") {
    val tradeDetailsPanel = new TradeDetailsPanel(tradeSelection)
    val tradeDetailsScrollPane = new ScrollPane(tradeDetailsPanel) {
      peer.getVerticalScrollBar.setUnitIncrement(10)
      peer.getHorizontalScrollBar.setUnitIncrement(10)
      border = EmptyBorder
      preferredSize = tradeDetailsPanel.preferredSize
    }
    add(LabelWithSeparator("Trade Details"), "spanx, growx, wrap")
    add(tradeDetailsScrollPane, "skip 1")
  }

  def shouldUpdateTradeExpiryChooser(chooser:ComboBox[TradeExpiryDay]) = {
    val selectedDay = chooser.selection.item.exp
    checkDay(selectedDay)
  }

  def checkDay(day:Day) = {
    !((day.dayNumber == 1) && (day.month == 10))
  }

  def generateEnvironmentChooserComponent(titleText:String) = {
    val marketDataChooser = new MarketDataChooser(tradeSelection.desk, context, None)
    val tradeTimestampChooser = new TimestampChooser(tradeTimestamp, desk, context)
    val tradeExpiryChooser = new ComboBox(TradeExpiryDay.types) {renderer = ListView.Renderer(_.toString)}
    if (checkDay(tradeExpiryDay)) {
      tradeExpiryChooser.selection.item = TradeExpiryDay(marketDataChooser.day)
    } else {
      tradeExpiryChooser.selection.item = TradeExpiryDay(tradeExpiryDay)
    }
    val panel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][fill,grow][p]", "[p]") {
      add(LabelWithSeparator(titleText), "spanx, growx, wrap")
      add(marketDataChooser, "skip 1")
      val tradeDaysPanel = new MigPanel("insets 0") {
        add(new Label("Book close"))
        add(tradeTimestampChooser, "growx, gapright " + RightPanelSpace + ", wrap")
        add(new Label("Trades live on:"))
        add(tradeExpiryChooser, "growx, gapright " + RightPanelSpace)

        reactions += {
          case ObservationDayChanged(`marketDataChooser`, d) if (shouldUpdateTradeExpiryChooser(tradeExpiryChooser)) => {
            tradeExpiryChooser.selection.item = TradeExpiryDay(d)
          }
        }
        listenTo(marketDataChooser)
      }
      add(tradeDaysPanel, "ay top")
    }
    (panel, marketDataChooser, tradeTimestampChooser, tradeExpiryChooser)
  }

  def generateSecondDayEnvironmentChooserComponent(fromMarketDataChooser:MarketDataChooser, fromTradeExpiryChooser:ComboBox[TradeExpiryDay], titleText:String, showValuationDayPicker:Boolean=false) = {
    val marketDataChooser = new MarketDataChooser(tradeSelection.desk, context, None, showValuationDayPicker)
    val tradeTimestampChooser = new TimestampChooser(tradeTimestamp, desk, context)
    val tradeExpiryChooser = new ComboBox(TradeExpiryDay.types) {
      enabled = false
      renderer = ListView.Renderer(_.toString)
    }
    if (checkDay(tradeExpiryDay)) {
      tradeExpiryChooser.selection.item = TradeExpiryDay(marketDataChooser.day)
    } else {
      tradeExpiryChooser.selection.item = TradeExpiryDay(tradeExpiryDay)
    }
    val panel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][fill,grow][p]", "[p]") {
      add(LabelWithSeparator(titleText), "spanx, growx, wrap")
      add(marketDataChooser, "skip 1")
      val tradeDaysPanel = new MigPanel("insets 0") {
        add(new Label("Book close:"))
        add(tradeTimestampChooser, "growx, gapright " + RightPanelSpace + ", wrap")
        add(new Label("Trades live on:"))
        add(tradeExpiryChooser, "growx, gapright " + RightPanelSpace)

        reactions += {
          case ObservationDayChanged(`fromMarketDataChooser`, d) if (shouldUpdateTradeExpiryChooser(tradeExpiryChooser)) => {
            tradeExpiryChooser.selection.item = TradeExpiryDay(d)
          }
          case SelectionChanged(`fromTradeExpiryChooser`) => {
            tradeExpiryChooser.selection.item = fromTradeExpiryChooser.selection.item
          }
        }
        listenTo(fromMarketDataChooser, fromTradeExpiryChooser.selection)
      }
      add(tradeDaysPanel, "ay top")
    }
    (panel, marketDataChooser, tradeTimestampChooser, tradeExpiryChooser)
  }

  val reportOptionsAvailable = context.localCache.reportOptionsAvailable

  def generateCheckBoxPanel(pivotReportTypes:List[PivotReportTypeLabel]) = new MigPanel("insets 0, wrap 2, gapy 0", "[p]unrel[p]") {
    val checkBoxes = new ListBuffer[(CheckBox, Label)]
    for (reportOption <- pivotReportTypes) {
      val fieldLabel = new Label(reportOption.fieldNames.mkString(", ")) {
        tooltip = "The fields that will be generated by the report"
        enabled = false
      }
      val checkBox = new CheckBox(reportOption.name) {
        selected = false
        reactions += {
          case ButtonClicked(b) => fieldLabel.enabled = selected
        }
      }
      reactions += {
        case MouseClicked(`checkBox`, _, _, 2, _) => {
          checkBoxes.filterNot(_._1 == checkBox).foreach {case (cb, la) => cb.selected = false; la.enabled = false}
          checkBox.selected = true
          fieldLabel.enabled = true
        }
      }
      listenTo(checkBox.mouse.clicks)
      checkBoxes += ((checkBox, fieldLabel))

      add(checkBox)
      add(fieldLabel)
    }
    // This is a big hack to just select position report first. This is the most useful report for Jon.
    checkBoxes.foreach {case (cb, lb) => if (cb.text == "Greeks") {cb.selected = true; lb.enabled = true}}

    def reportOptions = {
      val fullReportOptions = reportOptionsAvailable.options
      val reportOptionNames = checkBoxes.filter(_._1.selected).map(_._1.text)
      val selectedReportOptions = fullReportOptions.flatMap(reportOption => {
        if (reportOptionNames.contains(reportOption.name)) {
          Some(reportOption)
        } else {
          None
        }
      })
      ReportOptions(selectedReportOptions, None, None)
    }

    def reportOptions_=(ro:ReportOptions) {
      val names = ro.options.map(_.name).toSet
      checkBoxes.foreach(cb => {
        val contains = names.contains(cb._1.text)
        cb._1.selected = contains
        cb._2.enabled = contains
      })
    }
  }

  val standardReportsButton = new RadioButton("Standard Reports")
  val slideReportsButton = new RadioButton("Slide Reports")
  val pnlExplanationReportButton = new RadioButton("Day Change Explanation Report")
  val tradeChangesReportButton = new RadioButton("Trade Changes Report")
  val differenceReportsButton = new RadioButton("Difference Reports")
  val pnlDifferenceReportButton = new RadioButton("PnL Difference Report")
  val buttonGroup = new ButtonGroup(standardReportsButton, slideReportsButton,
    pnlExplanationReportButton, tradeChangesReportButton, differenceReportsButton, pnlDifferenceReportButton)
  buttonGroup.select(standardReportsButton)

  val (slidableOptions, nonSlidableOptions) = reportOptionsAvailable.options.partition(_.slidable)

  val standardReportsPanel = new ReportPanel {
    val (envChooserPanel, envChooser, tradeTSC, tradeLOC) = generateEnvironmentChooserComponent("Valuation Environment")
    val comp = new MigPanel("insets 0") {
      val checkBoxPanel = generateCheckBoxPanel(nonSlidableOptions ::: slidableOptions)

      val reportTypesPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][fill,grow]") {
        add(LabelWithSeparator("Report Types"), "spanx, growx, wrap")
        add(checkBoxPanel, "skip 1")
      }

      reactions += {
        case ViewRequested(`envChooser`, mod) => {
          envChooser.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(StandardMarketDataPageIdentifier(curveIdentifier.marketDataIdentifier), MarketDataPageState()), mod)
          })
        }
      }
      listenTo(envChooser)

      def reportOptions = checkBoxPanel.reportOptions

      add(reportTypesPanel, "growx, wrap")
      add(generateTradeDetailsComponent, "pushx, growx, wrap")
      add(envChooserPanel, "pushx, growx")
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      val tradeED = tradeLOC.selection.item.exp
      val tradeTS = tradeTSC.selectedTimestamp
      val tradeSelectionWithTimestamp = tradeSelection.withTimestamp(tradeTS, context.localCache.latestTimestamp)
      envChooser.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
        context.goTo({
          val reportParameters = ReportParameters(tradeSelectionWithTimestamp, curveIdentifier, comp.reportOptions,
            tradeED)
          new MainPivotReportPage(false, reportParameters, PivotPageState(false, PivotFieldParams(true, None)))
        }, modifiers = modifiers)
      })
    }

    override def componentState = StandardReportCompState(comp.checkBoxPanel.reportOptions, envChooser.getState, tradeTSC.selectedTimestamp, tradeLOC.selection.item)

    override def componentState_=(cs:ComponentState) = {
      cs match {
        case StandardReportCompState(ro, cs, tradesAsOf, tradesLiveOn) => {
          comp.checkBoxPanel.reportOptions = ro
          envChooser.setState(cs, false)
          tradeTSC.selectedTimestamp = tradesAsOf
          tradeLOC.selection.item = tradesLiveOn
        }
        case _ =>
      }
    }
  }

  val slideReportsPanel = new ReportPanel {
    val (envChooserPanel, envChooser, tradeTSC, tradeLOC) = generateEnvironmentChooserComponent("Valuation Environment")
    val comp = new MigPanel("insets 0") {
      val checkBoxPanel = generateCheckBoxPanel(slidableOptions)
      val slide1DPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
        val slideLabel = new Label("1D Slide")
        val slidePanel = new SlideOptionsPanel
        add(slideLabel, "spanx, wrap")
        add(slidePanel, "skip 1")
      }

      val slide2DPanel = new MigPanel("insets 0 10lp 0 0", "[" + StandardLeftIndent + "][p]") {
        val slideEnabledCheckBox = new CheckBox("2D Slide")
        val slidePanel = new SlideOptionsPanel
        slidePanel.visible = false

        reactions += {
          case ButtonClicked(`slideEnabledCheckBox`) => slidePanel.visible = slideEnabledCheckBox.selected
        }
        listenTo(slideEnabledCheckBox)

        add(slideEnabledCheckBox, "spanx, wrap")
        add(slidePanel, "skip 1")
      }

      val reportTypesPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
        add(LabelWithSeparator("Report Types"), "spanx, growx, wrap")
        add(checkBoxPanel, "skip 1, wrap unrel")
        add(slide1DPanel, "skip 1, split, spanx")
        add(slide2DPanel, "gapbefore 20lp, pushx, growx")
      }

      def reportOptions = {
        val checkBoxRO = checkBoxPanel.reportOptions
        val slide1 = Some(slide1DPanel.slidePanel.slideParameters)
        val slide2 = if (slide2DPanel.slideEnabledCheckBox.selected) {
          Some(slide2DPanel.slidePanel.slideParameters)
        } else {
          None
        }
        checkBoxRO.copy(slide1 = slide1, slide2 = slide2)
      }

      def reportOptions_=(ro:ReportOptions) {
        checkBoxPanel.reportOptions = ro
        ro.slide1.map(slide1DPanel.slidePanel.slideParameters = _)
        ro.slide2 match {
          case Some(slide2) => {
            slide2DPanel.slideEnabledCheckBox.selected = true
            slide2DPanel.slidePanel.visible = true
            slide2DPanel.slidePanel.slideParameters = slide2
          }
          case None => {
            slide2DPanel.slideEnabledCheckBox.selected = false
            slide2DPanel.slidePanel.visible = false
          }
        }
      }

      reactions += {
        case ViewRequested(`envChooser`, mod) => {
          envChooser.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(StandardMarketDataPageIdentifier(curveIdentifier.marketDataIdentifier), MarketDataPageState()), mod)
          })
        }
      }
      listenTo(envChooser)

      add(reportTypesPanel, "pushx, growx, wrap")
      add(generateTradeDetailsComponent, "pushx, growx, wrap")
      add(envChooserPanel, "pushx, growx")
    }

    class SlideOptionsPanel extends MigPanel("insets 0", "[p][fill][p]") {
      val numFieldCols = 5
      val slideParametersAvailable = reportOptionsAvailable.slideParameters
      val slideComboBox = new ComboBox(slideParametersAvailable.map(_.slideType))

      val marketComboBoxModel = new DefaultComboBoxModel
      val marketComboBox = new ComboBox(List("")) {
        peer.setModel(marketComboBoxModel)
      }
      val actualType = new Label("Parallel") {
        horizontalAlignment = Alignment.Left
      }
      val stepSizeField = new TextField(numFieldCols)
      val stepUOMLabel = new Label("")
      val upStepsField = new TextField(numFieldCols)
      val downStepsField = new TextField(numFieldCols)

      def marketsAvailable(slideType: String): Option[scala.List[SlideAttributes]] = {
        var marketsAvailable: Option[scala.List[SlideAttributes]] = None

        this.suppressing(marketComboBox.selection) {
          val slideParametersSelected = slideParametersAvailable.find(_.slideType == slideType).get

          upStepsField.text = slideParametersSelected.upSteps
          downStepsField.text = slideParametersSelected.downSteps
          // This is a bit of a hack as I'm just using the text here.
          downStepsField.enabled = (slideType != "Time")
          marketsAvailable = slideParametersSelected.markets
          marketsAvailable match {
            case None => {
              marketComboBoxModel.removeAllElements
              stepSizeField.text = slideParametersSelected.defaultStepSize.getOrElse("")
              stepUOMLabel.text = slideParametersSelected.defaultUOM.getOrElse("")
              marketComboBox.enabled = false
            }
            case Some(markets) => {
              marketComboBoxModel.removeAllElements
              markets.foreach(mar => marketComboBoxModel.addElement(mar.marketOrCommodity))
              marketComboBox.enabled = true
            }
          }
        }

        marketsAvailable
      }

      def slideTypeChanged(slideType:String) {
        // Update the market combo box and step details.
        marketsAvailable(slideType) match {
          case Some(m) => marketComboBox.selection.index = 0
          case None =>
        }
      }

      add(new Label("Slide:"))
      add(slideComboBox, "spanx, wrap")
      add(new Label("Market:"))
      add(marketComboBox, "spanx, wrap")
      add(new Label("Type:"))
      add(actualType, "spanx, wrap")
      add(new Label("Step Size:"))
      add(stepSizeField)
      add(stepUOMLabel, "wrap")
      add(new Label("Up Steps:"))
      add(upStepsField, "wrap")
      add(new Label("Down Steps:"))
      add(downStepsField)

      reactions += {
        case SelectionChanged(`slideComboBox`) => {slideTypeChanged(slideComboBox.selection.item)}
        case SelectionChanged(`marketComboBox`) => {
          val market = marketComboBox.selection.item
          // There must be a market.
          val slideAttributes = slideParametersAvailable.flatMap(_.markets).flatten.filter(_.marketOrCommodity == market)(0)
          stepSizeField.text = slideAttributes.stepSize
          slideAttributes.uom match {
            case Some(unit) => stepUOMLabel.text = unit
            case _ =>
          }
        }
      }
      listenTo(slideComboBox.selection)
      slideTypeChanged(slideParametersAvailable(0).slideType)

      def slideParameters = {
        val market = if (marketComboBox.enabled) {
          Some(marketComboBox.selection.item)
        } else {
          None
        }
        SlideParametersLabel(slideComboBox.selection.item, market, stepSizeField.text.trim.toDouble,
          upStepsField.text.trim.toInt, downStepsField.text.trim.toInt, stepUOMLabel.text, None, "")
      }

      def slideParameters_=(sp:SlideParametersLabel) {
        slideComboBox.selection.item = sp.slideType
        sp.market.map(m => marketComboBox.selection.item = m)
        sp.commodity.map(c => marketComboBox.selection.item = c)
        stepSizeField.text = sp.stepSize.toString
        upStepsField.text = sp.upSteps.toString
        downStepsField.text = sp.downSteps.toString
        stepUOMLabel.text = sp.uom
      }
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      val tradeED = tradeLOC.selection.item.exp
      val tradeTS = tradeTSC.selectedTimestamp
      val tradeSelectionWithTimestamp = tradeSelection.withTimestamp(tradeTS, context.localCache.latestTimestamp)
      envChooser.invokeWithCurveIdentifier(context, (curveID:CurveIdentifierLabel) => {
        context.goTo({
          val reportParameters = ReportParameters(tradeSelectionWithTimestamp, curveID, comp.reportOptions,
            tradeED)
          new MainPivotReportPage(false, reportParameters, PivotPageState(false, PivotFieldParams(true, None)))
        }, modifiers = modifiers)
      })
    }

    override def componentState = SlideReportCompState(comp.reportOptions, envChooser.getState,
      tradeTSC.selectedTimestamp, tradeLOC.selection.item)
    override def componentState_=(cs:ComponentState) = {
      cs match {
        case SlideReportCompState(ro, cs, tradesAsOf, tradesLiveOn) => {
          comp.reportOptions = ro
          envChooser.setState(cs, false)
          tradeTSC.selectedTimestamp = tradesAsOf
          tradeLOC.selection.item = tradesLiveOn
        }
        case _ =>
      }
    }
  }

  val pnlExplanationReportPanel = new ReportPanel {
    val (envChooserPanelFrom, envChooserFrom, tradeTSFrom, tradeLOCFrom) = generateEnvironmentChooserComponent("Valuation Environment for Day \"from\"")
    val (envChooserPanelTo, envChooserTo, tradeTSTo, tradeLOCTo) = generateSecondDayEnvironmentChooserComponent(envChooserFrom, tradeLOCFrom, "Valuation Environment for Day \"to\"", true)
    val checkBoxPanel = generateCheckBoxPanel(slidableOptions)
    val comp = new MigPanel("insets 0") {
      reactions += {
        case ViewRequested(`envChooserFrom`, mod) => {
          envChooserFrom.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(curveIdentifier.marketDataIdentifier, MarketDataPageState()), mod)
          })
        }
        case ViewRequested(`envChooserTo`, mod) => {
          envChooserTo.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(curveIdentifier.marketDataIdentifier, MarketDataPageState()), mod)
          })
        }
      }
      listenTo(envChooserFrom, envChooserTo)

      // The "from" chooser should be a day behind the "to" chooser.
      envChooserFrom.day = envChooserFrom.day.previousBusinessDay(context.localCache.ukBusinessCalendar)

      add(generateTradeDetailsComponent, "pushx, growx, wrap")
      add(checkBoxPanel, "growx, wrap")
      add(envChooserPanelFrom, "growx, wrap")
      add(envChooserPanelTo, "growx")
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      val tsFrom = tradeSelection.desk.map(m => tradeTSFrom.selectedTimestamp)
      val tsTo = tradeTSTo.selectedTimestamp
      val tradeSelectionWithTimestamp = tradeSelection.withTimestamp(tsTo, context.localCache.latestTimestamp)
      envChooserFrom.invokeWithCurveIdentifier(context, curveIdentifier1 => {
        envChooserTo.invokeWithCurveIdentifier(context, curveIdentifier2 => {
          context.goTo({
            val reportParameters = new ReportParameters(
              tradeSelectionWithTimestamp,
              curveIdentifier2,
              checkBoxPanel.reportOptions,
              tradeLOCFrom.selection.item.exp,
              Some( PnlFromParameters(tsFrom, curveIdentifier1))
            )
            MainPivotReportPage(
              false,
              reportParameters,
              PivotPageState(false, PivotFieldParams(true, None))
            )
          }, modifiers = modifiers)
        })
      })
    }

    override def componentState = PnLExplanationReportCompState(envChooserFrom.getState, tradeTSFrom.selectedTimestamp, tradeLOCFrom.selection.item,
      envChooserTo.getState, tradeTSTo.selectedTimestamp, tradeLOCTo.selection.item)
    override def componentState_=(cs:ComponentState) = {
      cs match {
        case PnLExplanationReportCompState(envFrom, asOfFrom, liveOnFrom, envTo, asOfTo, liveOnTo) => {
          envChooserFrom.setState(envFrom, false)
          tradeTSFrom.selectedTimestamp = asOfFrom
          tradeLOCFrom.selection.item = liveOnFrom
          envChooserTo.setState(envTo, false)
          tradeTSTo.selectedTimestamp = asOfTo
          tradeLOCTo.selection.item = liveOnTo
        }
        case _ =>
      }
    }
  }

  val tradeChangesReportPanel = new ReportPanel {
    val timestampChooserFrom = new TimestampChooser(tradeTimestamp, desk, context)
    // For the moment, say the from chooser should be the second selection.
    val timestampChooserTo = new TimestampChooser(tradeTimestamp, desk, context)
    val comp = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p][fill,grow]") {
      add(generateTradeDetailsComponent, "spanx, growx, wrap")
      add(LabelWithSeparator("Trade Environment"), "spanx, growx, wrap")
      add(new Label("Trades \"from\" as of:"), "skip 1")
      add(timestampChooserFrom, "wrap")
      add(new Label("Trades \"to\" as of:"), "skip 1")
      add(timestampChooserTo)
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      context.goTo(TradeChangesReportPage(tradeSelection,
        timestampChooserFrom.selectedTimestamp, timestampChooserTo.selectedTimestamp,
        PivotPageState(false, PivotFieldParams(true, None)), tradeExpiryDay), modifiers = modifiers)
    }

    override def componentState = TradeChangesCompState(timestampChooserFrom.selectedTimestamp, timestampChooserTo.selectedTimestamp)
    override def componentState_=(cs:ComponentState) = {
      cs match {
        case TradeChangesCompState(from, to) => {
          timestampChooserFrom.selectedTimestamp = from
          timestampChooserTo.selectedTimestamp = to
        }
        case _ =>
      }
    }
  }

  val differenceReportPanel = new ReportPanel {
    val (envChooserPanelFrom, envChooserFrom, tradeTSFrom, tradeLOCFrom) = generateEnvironmentChooserComponent("Valuation Environment for Day \"from\"")
    val (envChooserPanelTo, envChooserTo, tradeTSTo, tradeLOCTo) = generateSecondDayEnvironmentChooserComponent(envChooserFrom, tradeLOCFrom, "Valuation Environment for Day \"to\"")

    val comp = new MigPanel("insets 0") {
      val checkBoxPanel = generateCheckBoxPanel(nonSlidableOptions ::: slidableOptions)

      val reportTypesPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][fill,grow]") {
        add(LabelWithSeparator("Report Types"), "spanx, growx, wrap")
        add(checkBoxPanel, "skip 1")
      }

      reactions += {
        case ViewRequested(`envChooserFrom`, mod) => {
          envChooserFrom.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(curveIdentifier.marketDataIdentifier, MarketDataPageState()), mod)
          })
        }
        case ViewRequested(`envChooserTo`, mod) => {
          envChooserTo.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(curveIdentifier.marketDataIdentifier, MarketDataPageState()), mod)
          })
        }
      }
      listenTo(envChooserFrom)
      listenTo(envChooserTo)

      // The "from" chooser should be a day behind the "to" chooser.
      envChooserFrom.day = envChooserFrom.day.previousBusinessDay(context.localCache.ukBusinessCalendar)

      def reportOptions = checkBoxPanel.reportOptions

      add(reportTypesPanel, "growx, wrap")
      add(generateTradeDetailsComponent, "pushx, growx, wrap")
      add(envChooserPanelFrom, "growx, wrap")
      add(envChooserPanelTo, "growx")
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      envChooserFrom.invokeWithCurveIdentifier(context, curveIdentifier1 => {
        envChooserTo.invokeWithCurveIdentifier(context, curveIdentifier2 => {
          context.goTo(DifferenceMainPivotReportPage(
            tradeSelection, curveIdentifier1, curveIdentifier2, comp.reportOptions,
            PivotPageState(false, PivotFieldParams(true, None)),
            tradeTSFrom.selectedTimestamp, tradeTSTo.selectedTimestamp,
            tradeLOCFrom.selection.item.exp), modifiers = modifiers)
        })
      })
    }

    override def componentState = DifferenceReportCompState(comp.checkBoxPanel.reportOptions,
      envChooserFrom.getState, tradeTSFrom.selectedTimestamp, tradeLOCFrom.selection.item,
      envChooserTo.getState, tradeTSTo.selectedTimestamp, tradeLOCTo.selection.item)

    override def componentState_=(cs:ComponentState) = {
      cs match {
        case DifferenceReportCompState(reportOpts, envFrom, asOfFrom, liveOnFrom, envTo, asOfTo, liveOnTo) => {
          comp.checkBoxPanel.reportOptions = reportOpts
          envChooserFrom.setState(envFrom, false)
          tradeTSFrom.selectedTimestamp = asOfFrom
          tradeLOCFrom.selection.item = liveOnFrom
          envChooserTo.setState(envTo, false)
          tradeTSTo.selectedTimestamp = asOfTo
          tradeLOCTo.selection.item = liveOnTo
        }
        case _ =>
      }
    }
  }

  val pnlDifferenceReportPanel = new ReportPanel {
    val (envChooserPanel, envChooser, tradeTSC, tradeLOC) = generateEnvironmentChooserComponent("Valuation Environment")
    val comp = new MigPanel("insets 0") {
      reactions += {
        case ViewRequested(`envChooser`, mod) => {
          envChooser.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
            context.goTo(new MarketDataPage(curveIdentifier.marketDataIdentifier, MarketDataPageState()), mod)
          })
        }
      }
      listenTo(envChooser)

      add(generateTradeDetailsComponent, "pushx, growx, wrap")
      add(envChooserPanel, "pushx, growx")
    }

    def component = comp

    def runReport(modifiers:Modifiers) {
      val tradeED = tradeLOC.selection.item.exp // expiry day
      val tradeTS = tradeTSC.selectedTimestamp // timestamp
      val tradeSelectionWithTimestamp = tradeSelection.withTimestamp(tradeTS, context.localCache.latestTimestamp)
      envChooser.invokeWithCurveIdentifier(context, (curveIdentifier:CurveIdentifierLabel) => {
        context.goTo({
          new PnLReconciliationReportPage(tradeSelectionWithTimestamp, curveIdentifier, tradeED, PivotPageState(false, PivotFieldParams(true, None)))
        }, modifiers = modifiers)
      })
    }

    override def componentState = PnLDifferenceReportCompState(envChooser.getState, tradeTSC.selectedTimestamp, tradeLOC.selection.item)

    override def componentState_=(cs:ComponentState) = {
      cs match {
        case PnLDifferenceReportCompState(cs, tradesAsOf, tradesLiveOn) => {
          envChooser.setState(cs, false)
          tradeTSC.selectedTimestamp = tradesAsOf
          tradeLOC.selection.item = tradesLiveOn
        }
        case _ =>
      }
    }
  }


  val reportOptionsPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][fill,grow]", "[fill,grow]") {
    var currentReportPanel:ReportPanel = null

    def update(reportPanel:ReportPanel) {
      currentReportPanel = reportPanel
      removeAll
      add(reportPanel.component, "skip 1")
      revalidate
      repaint
    }
  }

  val runReportButton = new NewPageButton {
    text = "Run Report"
    tooltip = "Runs a report using the options specified"
    reactions += {
      case ButtonClickedEx(b, e) => {reportOptionsPanel.currentReportPanel.runReport(Modifiers.modifiers(e.getModifiers))}
    }
  }

  val contents0 = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]20lp[p]20lp[p]20lp[p]20lp[grow]") {
    add(LabelWithSeparator("Report Configuration"), "spanx, growx, wrap")
    add(standardReportsButton, "skip 1")
    add(slideReportsButton)
    add(pnlExplanationReportButton)
    add(tradeChangesReportButton)
    add(differenceReportsButton)
    add(pnlDifferenceReportButton, "wrap unrel")
    add(reportOptionsPanel, "skip 1, spanx, push, grow, wrap")
    add(runReportButton, "split, spanx, al right, gapright " + RightPanelSpace)
  }

  reportOptionsPanel.update(standardReportsPanel)

  val buttonToPanel = Map[AbstractButton,ReportPanel](
    standardReportsButton -> standardReportsPanel,
    slideReportsButton -> slideReportsPanel,
    pnlExplanationReportButton -> pnlExplanationReportPanel,
    tradeChangesReportButton -> tradeChangesReportPanel,
    differenceReportsButton -> differenceReportPanel,
    pnlDifferenceReportButton -> pnlDifferenceReportPanel
  )

  reactions += {
    case ButtonClicked(b) => {reportOptionsPanel.update(buttonToPanel(b))}
  }
  listenTo(standardReportsButton, slideReportsButton, pnlExplanationReportButton, tradeChangesReportButton,
    differenceReportsButton, pnlDifferenceReportButton)

  add(contents0, BorderPanel.Position.Center)

  override def getState = {
    val buttonSelected = buttonGroup.selected.get
    val panelState = buttonToPanel(buttonSelected).componentState
    Some(ReportConfiguration(buttonSelected.text, panelState))
  }

  override def setState(state:Option[ComponentState]) = {
    state match {
      case Some(rc:ReportConfiguration) => {
        val buttonToSelect = buttonGroup.buttons.find(_.text == rc.reportType).get
        buttonGroup.select(buttonToSelect)
        val reportPanel = buttonToPanel(buttonToSelect)
        reportPanel.componentState = rc.state
        reportOptionsPanel.update(reportPanel)
      }
      case _ =>
    }
  }
}

case class ReportConfiguration(reportType:String, state:ComponentState) extends ComponentState

case class StandardReportCompState(reportOptions:ReportOptions, chooserState:MarketDataChooserState, tradesAsOf:TradeTimestamp, tradeExpiryDay:TradeExpiryDay) extends ComponentState
case class SlideReportCompState(reportOptions:ReportOptions, chooserState:MarketDataChooserState, tradesAsOf:TradeTimestamp,
                                tradeExpiryDay:TradeExpiryDay) extends ComponentState
case class PnLExplanationReportCompState(fromChooserState:MarketDataChooserState, fromTradesAsOf:TradeTimestamp, fromTradeExpiryDay:TradeExpiryDay,
                                         toChooserState:MarketDataChooserState, toTradesAsOf:TradeTimestamp, toTradeExpiryDay:TradeExpiryDay) extends ComponentState
case class TradeChangesCompState(tradesFromAsOf:TradeTimestamp, tradesToAsOf:TradeTimestamp) extends ComponentState
case class DifferenceReportCompState(reportOptions:ReportOptions, fromChooserState:MarketDataChooserState, fromTradesAsOf:TradeTimestamp, fromTradeExpiryDay:TradeExpiryDay,
                                     toChooserState:MarketDataChooserState, toTradesAsOf:TradeTimestamp, toTradeExpiryDay:TradeExpiryDay) extends ComponentState
case class PnLDifferenceReportCompState(chooserState:MarketDataChooserState, tradesAsOf:TradeTimestamp, tradeExpiryDay:TradeExpiryDay) extends ComponentState

trait ReportPanel {
  def component:Component
  def runReport(modifiers:Modifiers)
  def componentState:ComponentState = new ComponentState {}
  def componentState_=(cs:ComponentState) {}
}