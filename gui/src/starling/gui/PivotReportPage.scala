package starling.gui

import api._
import pages._
import swing._
import event.{Event, ListSelectionChanged, ButtonClicked}
import starling.pivot._
import collection.mutable.ListBuffer
import java.awt.datatransfer.StringSelection
import starling.daterange.{Day}
import java.awt.{Toolkit, Dimension}
import javax.swing.ImageIcon
import starling.browser._
import common.{NListView, MigPanel}

class PivotReportPage {}

abstract class AbstractPivotReportPage(
        reportParameters:ReportParameters,
        pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
}

case class DifferenceMainPivotReportPage(
        tradeSelection:TradeSelection,
        curveIdentifierDm1:CurveIdentifierLabel,
        curveIdentifierD:CurveIdentifierLabel,
        reportOptions:ReportOptions,
        pivotPageState:PivotPageState,
        fromTimestamp: TradeTimestamp,
        toTimestamp: TradeTimestamp,
        expiryDay: Day
        ) extends AbstractStarlingPivotPage(pivotPageState) {

  assert(tradeSelection.intradaySubgroup.isEmpty, "Difference reports don't work with Excel trades")

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.cachingStarlingServer.diffReportPivot(tradeSelection, curveIdentifierDm1, curveIdentifierD,
      reportOptions, expiryDay, fromTimestamp, toTimestamp, pivotPageState.pivotFieldParams)
  }

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event,Page]]
    functions.toList
  }

  def text = tradeSelection + " " + curveIdentifierD + " vs " + curveIdentifierDm1
  def selfPage(pps:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pps)
  override def toolbarButtons(pageContext:PageContext, data:PageData) =
    PivotReportPage.toolbarButtons(
      pageContext,
      ReportParameters(tradeSelection.withDeskTimestamp(toTimestamp), curveIdentifierD, reportOptions, expiryDay),
      data,
      false,
      pivotPageState)
}

case class PivotReportTablePageData(numErrors:Int) extends PageData

case class MainPivotReportPage(showParameters:Boolean, reportParameters:ReportParameters, pivotPageState:PivotPageState) extends AbstractStarlingPivotPage(pivotPageState) {
  private def shortTitle = "Risk Report"
  def text = if (showParameters) shortTitle else reportParameters.text
  override def icon = StarlingIcons.im("/icons/16x16_report.png")
  override def shortText = if (showParameters) shortTitle else reportParameters.shortText

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event,Page]]
    reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp match {
      case Some((groups, _)) => functions += {
        case IntradayUpdated(group, _, timestamp) if groups.subgroups.contains(group) => selfReportPage(reportParameters.copyWithIntradayTimestamp(timestamp))
      }
      case _ =>
    }
    val excelNames =
      reportParameters.curveIdentifier.marketDataIdentifier.selection.excel.toList :::
      reportParameters.pnlParameters.toList.flatMap {
        pnlFrom => pnlFrom.curveIdentifierFrom.marketDataIdentifier.selection.excel.toList
      }

    if (!excelNames.isEmpty) {
      functions += { case ExcelMarketDataUpdate(name, version) if (excelNames.contains(name)) => {
        val pnlParameters = reportParameters.pnlParameters.map {
          pnlParameters => {
            pnlParameters.curveIdentifierFrom.marketDataIdentifier.selection.excel match {
              case Some(`name`) => pnlParameters.copy(curveIdentifierFrom=pnlParameters.curveIdentifierFrom.copyVersion(version))
              case _ => pnlParameters
            }
          }
        }
        val curveIdentifier = reportParameters.curveIdentifier.marketDataIdentifier.selection.excel match {
          case Some(`name`) => reportParameters.curveIdentifier.copyVersion(version)
          case None => reportParameters.curveIdentifier
        }
        selfReportPage(reportParameters.copy(curveIdentifier=curveIdentifier, pnlParameters=pnlParameters))
      } }
    }

    //TODO [02 Dec 2010] respond to PricingGroup market data changes

    functions.toList
  }

  override def finalDrillDownPage(fields:scala.Seq[(Field, Selection)], pageContext:PageContext, ctrlDown:Boolean) {
    val selection = fields.find(f=>f._1.name == "Trade ID")
    val tradeID = selection match {
      case Some( (field,selection)) => {
        selection match {
          case SomeSelection(values) if (values.size==1) => Some(values.toList(0).asInstanceOf[TradeIDLabel])
          case _ => None
        }
      }
      case None => None
    }
    tradeID match {
      case Some(trID) => {
        pageContext.goTo(ValuationParametersPage(trID, reportParameters), newTab = ctrlDown)
      }
      case None => None
    }
  }

  override def subClassesPageData(reader:StarlingServerContext):Option[PageData] = {
    Some(PivotReportTablePageData(reader.cachingStarlingServer.reportErrors(reportParameters).errors.size))
  }

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.cachingStarlingServer.reportPivot(reportParameters, pivotPageState.pivotFieldParams)
  }
  def selfPage(pps:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pps)
  def selfReportPage(rp:ReportParameters, pps:PivotPageState = pivotPageState) = copy(reportParameters = rp, pivotPageState = pps)
  override def toolbarButtons(pageContext:PageContext, data:PageData) =
    PivotReportPage.toolbarButtons(pageContext, reportParameters, data, showParameters, pivotPageState)

  override def configPanel(context:PageContext, data:PageData) = {
    if (showParameters) {
      val pivotData = data match {
        case PivotTablePageData(pivData,Some(pd)) => pd match {
          case PivotReportTablePageData(_) => {
            pivData
          }
          case scd => throw new Exception("Don't know how to handle this type of subclass page data: " + scd.getClass)
        }
        case PivotTablePageData(pivData,None) => pivData
        case _ => throw new Exception("Don't know how to handle this type of page data")
      }
      
      def update(rp: ReportParameters) {
        val optionsAvailable = pivotData.reportSpecificOptions.nonEmpty
        val noChoices = pivotPageState.pivotFieldParams.pivotFieldState match {
          case None => false // I don't think this can ever happen so I won't bother doing anything with it.
          case Some(pfs) => pfs.reportSpecificChoices.isEmpty
        }
        val newPivotPageState = if (optionsAvailable && noChoices) {
          // This is a massive hack to get the report specific choices into the page the first time the report is run.
          val newReportSpecificChoices = pivotData.pivotFieldsState.reportSpecificChoices
          val newPivotFieldState = pivotPageState.pivotFieldParams.pivotFieldState.get.copy(reportSpecificChoices = newReportSpecificChoices)
          val newPivotFieldParams = pivotPageState.pivotFieldParams.copy(pivotFieldState = Some(newPivotFieldState))
          pivotPageState.copy(pivotFieldParams = newPivotFieldParams)
        } else {
          pivotPageState
        }

        context.createAndGoTo{
          server => {
            // check to see if we have market data for the observation day and pnl from day, if we don't, import it
            // making new copies of the ReportParameters is the really ugly bit
            selfReportPage(rp = rp, pps = newPivotPageState)
          }
        }
      }

      val manualConfigPanel = new ManualReportConfigPanel(context, reportParameters, pivotPageState)
      val slideConfigPanel = new SlideReportConfigPanel(context, reportParameters)
      def generateRPs = {
        manualConfigPanel.generateReportParams(slideConfigPanel.slideConfig)
      }
      val presetReportPanel = new PresetReportConfigPanel(context, reportParameters, pivotPageState)
      val tradeInfoPanel = new TradeInfoConfigPanel(context, reportParameters)

      val runPanel = new MigPanel("insets 0","[p]1lp[p]push") {
        reactions += {
          case UpdateRunButtonEvent(`presetReportPanel`) => {
            val rps = presetReportPanel.generateReportParameters match {
              case None => generateRPs
              case Some(rp) => rp
            }
            deafTo(manualConfigPanel, slideConfigPanel)
            manualConfigPanel.setupState(rps)
            slideConfigPanel.setupState(rps)
            listenTo(manualConfigPanel, slideConfigPanel)
            updateRunButton(rps)
          }
          case UpdateRunButtonEvent(_) => {
            val rps = generateRPs
            deafTo(presetReportPanel)
            presetReportPanel.setupState(rps)
            listenTo(presetReportPanel)

            updateRunButton(rps)
          }
          case MarketDataChanged(`manualConfigPanel`) => {
            val mds = generateRPs.curveIdentifier.marketDataIdentifier.selection
            deafTo(presetReportPanel)
            presetReportPanel.marketData(mds)
            listenTo(presetReportPanel)
            updateRunButton(mds)
          }
          case MarketDataChanged(`presetReportPanel`) => {
            val mds = presetReportPanel.generateMarketDataIdentifier.selection
            deafTo(manualConfigPanel)
            manualConfigPanel.marketData(mds)
            listenTo(manualConfigPanel)
            updateRunButton(mds)
          }
        }
        listenTo(presetReportPanel, manualConfigPanel, slideConfigPanel)

        val runButton = new Button("Run") {
          tooltip = "Run the specified report (F9)"
          icon = StarlingIcons.icon("/icons/16x16_report.png")
          icon = new ImageIcon(getClass.getResource("/icons/16x16_report.png"))
          reactions += {
            case ButtonClicked(b) => run()
          }
        }

        def updateRunButton(rps:ReportParameters) {
          runButton.enabled = (rps != reportParameters || !reportParameters.runReports)
        }
        def updateRunButton(mds:MarketDataSelection) {
          runButton.enabled = (mds != reportParameters.curveIdentifier.marketDataIdentifier.selection || !reportParameters.runReports)
        }

        add(runButton, "ay center, tag ok")

        updateRunButton(generateRPs)

        def run() {
          pivotPageState.pivotFieldParams.pivotFieldState.map { fs => {
            val otherLayoutInfo = pivotPageState.otherLayoutInfo
            context.putSetting(StandardUserSettingKeys.DefaultReportFields, (fs,otherLayoutInfo))
          }}
          update(manualConfigPanel.generateReportParams(slideConfigPanel.slideConfig))
        }
      }

      Some(ConfigPanels(
        List(presetReportPanel, manualConfigPanel, slideConfigPanel, tradeInfoPanel),
        runPanel, Action("runReportAction") {runPanel.run()}))
    } else {
      None
    }
  }

  override def bookmark(serverContext:StarlingServerContext):Bookmark = ReportBookmark(showParameters, serverContext.server.createUserReport(reportParameters), pivotPageState)
}

case class ReportBookmark(showParameters:Boolean, userReportData:UserReportData, pivotPageState:PivotPageState) extends StarlingBookmark {
  def daySensitive = {
    userReportData.environmentRule match {
      case EnvironmentRuleLabel.RealTime => false
      case _ => true
    }
  }
  def createStarlingPage(day:Option[Day], serverContext:StarlingServerContext, context:PageContext) = {
    val dayToUse = day match {
      case None => Day.today() // Real time
      case Some(d) => d
    }
    val reportParameters = serverContext.server.createReportParameters(userReportData, dayToUse)
    MainPivotReportPage(showParameters, reportParameters, pivotPageState)
  }
}

object PivotReportPage {
  def toolbarButtons(pageContext:PageContext, reportParameters:ReportParameters, data:PageData, showParameters:Boolean,
                            pivotPageState:PivotPageState) = {
    val numErrors = data match {
      case PivotTablePageData(_,Some(pd)) => pd match {
        case PivotReportTablePageData(nErrs) => {
          nErrs
        }
        case scd => throw new Exception("Don't know how to handle this type of subclass page data: " + scd.getClass)
      }
      case PivotTablePageData(_,_) => 0
      case _ => throw new Exception("Don't know how to handle this type of page data")
    }
    
    val buffer = new ListBuffer[NewPageToolBarButton]()
    if (numErrors > 0) {
      buffer += new NewPageToolBarButton {
        text = "Errors (" + numErrors + ")"
        tooltip = "Show errors that occured whilst running this report"
        val leftIcon = StarlingIcons.im("/icons/error.png")
        reactions += {
          case ButtonClicked(b) => {
            pageContext.goTo(ReportErrorsPage(reportParameters))
          }
        }
      }
    }

    // Always add the market data button here.
    if (reportParameters.runReports) {
      buffer += new NewPageToolBarButton {
        text = "Market Data"
        tooltip = "Show all market data used to calculate the values shown"
        val leftIcon = StarlingIcons.im("/icons/16x16_market_data.png")
        focusable = false
        reactions += {
          case ButtonClicked(b) => {
            MarketDataPage.goTo(pageContext, ReportMarketDataPageIdentifier(reportParameters), None, None)
          }
        }
      }
    }

    buffer.toList
  }
}

case class ReportErrorsPage(reportParameters:ReportParameters) extends StarlingServerPage {
  def text = "Errors in " + reportParameters.text
  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = new PivotReportErrorPageComponent(context, data, browserSize, previousPageData)
  def build(pageBuildingContext: StarlingServerContext) = {
    val errors = pageBuildingContext.cachingStarlingServer.reportErrors(reportParameters)
    val errorsToUse = errors.errors.map(e => ErrorViewElement(e.instrumentText, e.message))
    PivotReportErrorPageData(errorsToUse)
  }
  def icon = StarlingIcons.im("/icons/error.png")
}
case class PivotReportErrorPageData(reportErrors:List[ErrorViewElement]) extends PageData


class PivotReportErrorPageComponent(pageContext:PageContext, data:PageData, browserSize:Dimension, previousPageData:Option[PageData]) extends MigPanel("") with PageComponent {
  val errors = data match {
    case d:PivotReportErrorPageData => d.reportErrors
  }
  val errorView = new ErrorView(errors, Some(scala.math.round(browserSize.height / 4.0f)))
  add(errorView, "push, grow")
}

case class ReportCellErrorsPage(errors:List[StackTrace]) extends StarlingServerPage {
  def text = "Errors"
  def icon = StarlingIcons.im("/icons/error.png")
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = {
    val errorsToUse = data match {
      case d:ReportCellErrorData => d.errors
    }
    new ReportCellErrorsPageComponent(errorsToUse, browserSize, previousPageData)
  }
  def build(pageBuildingContext:StarlingServerContext) = {ReportCellErrorData(errors.map(d => ErrorViewElement(d.message, d.stackTrace)))}
}
case class ReportCellErrorData(errors:List[ErrorViewElement]) extends PageData

class ReportCellErrorsPageComponent(errors:List[ErrorViewElement], browserSize:Dimension, previousPageData:Option[PageData]) extends MigPanel("") with PageComponent {
  val errorView = new ErrorView(errors, Some(scala.math.round(browserSize.height / 4.0f)))
  add(errorView, "push, grow")
}

case class ErrorViewElement(message:String, text:String)
class ErrorView(errors:List[ErrorViewElement], dividerLocation0:Option[Int] = None) extends MigPanel("insets 0") {
  val errorMessages = errors.map(_.message)
  val errorTexts = Map() ++ errors.map(e => (e.message -> e.text))
  val messagesView = new NListView(errorMessages)
  val messagesScroll = new ScrollPane(messagesView)
  val textArea = new TextArea {
    editable = false
  }
  val stackScrollPane = new ScrollPane(textArea)
  val copyToClipButton = new Button("Copy To Clipboard") {
    tooltip = "Copy all the text to the Clipboard"
  }
  reactions += {
    case ButtonClicked(`copyToClipButton`) => {
      val clip = Toolkit.getDefaultToolkit.getSystemClipboard
      val stringSelection = new StringSelection(textArea.text)
      clip.setContents(stringSelection, null)
    }
    case ListSelectionChanged(`messagesView`,_,false) => {
      textArea.text = errorTexts(messagesView.selected)
      textArea.peer.setCaretPosition(0)
    }
  }

  listenTo(copyToClipButton, messagesView.selection)

  val splitPane = new SplitPane(Orientation.Horizontal, messagesScroll, stackScrollPane) {
    oneTouchExpandable = true
    resizeWeight = 0.3
    if (dividerLocation0 != None) {
      dividerLocation = dividerLocation0.get
    }
    border = scala.swing.Swing.EmptyBorder
  }

  add(splitPane, "push, grow, wrap")
  add(copyToClipButton, "al right")

  messagesView.selectIndices(0)
  textArea.text = errorTexts(errorMessages(0))

  textArea.peer.setCaretPosition(0)
}
