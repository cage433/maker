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
import common.{ExButton, ButtonClickedEx, NListView, MigPanel}
import starling.gui.StarlingLocalCache._

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
    pageBuildingContext.reportService.diffReportPivot(tradeSelection, curveIdentifierDm1, curveIdentifierD,
      reportOptions, expiryDay, fromTimestamp, toTimestamp, pivotPageState.pivotFieldParams)
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

  override def latestPage(localCache:LocalCache) = {
    val page1 = reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp match {
      case Some((groups, _)) => {
        val latestTimestamp = localCache.latestTimestamp(groups)
        selfReportPage(reportParameters.copyWithIntradayTimestamp(latestTimestamp))
      }
      case None => this
    }

    val newPnlParameters:Option[PnlFromParameters] = page1.reportParameters.pnlParameters.map {
      pnlParameters => {
        localCache.latestMarketDataVersionIfValid(pnlParameters.curveIdentifierFrom.marketDataIdentifier.selection) match {
          case Some(v) => {
            pnlParameters.copy(curveIdentifierFrom=pnlParameters.curveIdentifierFrom.copyVersion(v))
          }
          case _ => pnlParameters
        }
      }
    }
    val newCurveIdentifier = localCache.latestMarketDataVersionIfValid(page1.reportParameters.curveIdentifier.marketDataIdentifier.selection) match {
      case Some(v) => {
        page1.reportParameters.curveIdentifier.copyVersion(v)
      }
      case _ => page1.reportParameters.curveIdentifier
    }

    page1.selfReportPage(page1.reportParameters.copy(curveIdentifier=newCurveIdentifier, pnlParameters=newPnlParameters))
  }

  override def finalDrillDownPage(fields:scala.Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers) {
    val selection = fields.find(f=>f._1.name == "Trade ID" && (f._2 match {
      case SomeSelection(vs) if vs.size == 1 => true
      case _ => false
    }))
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
        pageContext.goTo(ValuationParametersPage(trID, reportParameters), modifiers = modifiers)
      }
      case None => None
    }
  }

  override def subClassesPageData(reader:StarlingServerContext):Option[PageData] = {
    Some(PivotReportTablePageData(reader.reportService.reportErrors(reportParameters).errors.size))
  }

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    pageBuildingContext.reportService.reportPivot(reportParameters, pivotPageState.pivotFieldParams)
  }
  def selfPage(pps:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pps)
  def selfReportPage(rp:ReportParameters, pps:PivotPageState = pivotPageState) = copy(reportParameters = rp, pivotPageState = pps)
  override def toolbarButtons(pageContext:PageContext, data:PageData) =
    PivotReportPage.toolbarButtons(pageContext, reportParameters, data, showParameters, pivotPageState)

  override def configPanel(context:PageContext, data:PageData, tableSelection:() => TableSelection) = {
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
      
      def update(rp: ReportParameters, modifiers:Modifiers) {
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

        context.goTo(selfReportPage(rp = rp, pps = newPivotPageState), modifiers = modifiers)
      }

      val manualConfigPanel = new ManualReportConfigPanel(context, reportParameters, pivotPageState)
      val slideConfigPanel = new SlideReportConfigPanel(context, reportParameters)
      def generateRPs = {
        manualConfigPanel.generateReportParams(slideConfigPanel.slideConfig)
      }
      val presetReportPanel = new PresetReportConfigPanel(context, reportParameters, pivotPageState)
      val tradeInfoPanel = new TradeInfoConfigPanel(context, reportParameters)

      val runPanel = new MigPanel("insets 0") {
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

        val runButton = new ExButton("Run") {
          tooltip = "Run the specified report (F9)"
          icon = StarlingIcons.icon("/icons/16x16_report.png")
          icon = new ImageIcon(getClass.getResource("/icons/16x16_report.png"))
          reactions += {
            case ButtonClickedEx(b, e) => run(Modifiers.modifiers(e.getModifiers))
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

        def run(modifiers:Modifiers) {
          pivotPageState.pivotFieldParams.pivotFieldState.map { fs => {
            val otherLayoutInfo = pivotPageState.otherLayoutInfo
            context.putSetting(StandardUserSettingKeys.DefaultReportFields, (fs,otherLayoutInfo))
          }}
          update(manualConfigPanel.generateReportParams(slideConfigPanel.slideConfig), modifiers)
        }
      }

      Some(ConfigPanels(
        List(presetReportPanel, manualConfigPanel, slideConfigPanel, tradeInfoPanel),
        runPanel, Action("runReportAction") {runPanel.run(Modifiers.None)}))
    } else {
      None
    }
  }

  override def bookmark(serverContext:StarlingServerContext, pd:PageData):Bookmark = ReportBookmark(showParameters, serverContext.reportService.createUserReport(reportParameters), pivotPageState)
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
      case None => Day.today // Real time
      case Some(d) => d
    }
    val reportParameters = serverContext.reportService.createReportParameters(userReportData, dayToUse)
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
          case ButtonClickedEx(b, e) => {
            pageContext.goTo(ReportErrorsPage(reportParameters), Modifiers.modifiers(e.getModifiers))
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
          case ButtonClickedEx(b, e) => {
            MarketDataPage.goTo(pageContext, ReportMarketDataPageIdentifier(reportParameters), None, None, Modifiers.modifiers(e.getModifiers))
          }
        }
      }
    }

    buffer.toList
  }
}

case class ReportErrorsPage(reportParameters:ReportParameters) extends StarlingServerPage {
  def text = "Errors in " + reportParameters.text
  def createComponent(context: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new PivotReportErrorPageComponent(context, data, browserSize, previousPageData)
  def build(pageBuildingContext: StarlingServerContext) = {
    val errors = pageBuildingContext.reportService.reportErrors(reportParameters)
    val errorsToUse = errors.errors.map(e => ErrorViewElement(e.instrumentText, e.message))
    PivotReportErrorPageData(errorsToUse)
  }
  def icon = StarlingIcons.im("/icons/error.png")
}
case class PivotReportErrorPageData(reportErrors:List[ErrorViewElement]) extends PageData


class PivotReportErrorPageComponent(pageContext:PageContext, data:PageData, browserSize:Dimension, previousPageData:Option[PreviousPageData]) extends MigPanel("") with PageComponent {
  val errors = data match {
    case d:PivotReportErrorPageData => d.reportErrors
  }
  val errorView = new ErrorView(errors, Some(scala.math.round(browserSize.height / 4.0f)))
  add(errorView, "push, grow")
}

case class ReportCellErrorsPage(errors:List[StackTrace]) extends StarlingServerPage {
  def text = "Errors"
  def icon = StarlingIcons.im("/icons/error.png")
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = {
    val errorsToUse = data match {
      case d:ReportCellErrorData => d.errors
    }
    new ReportCellErrorsPageComponent(errorsToUse, browserSize, previousPageData)
  }
  def build(pageBuildingContext:StarlingServerContext) = {ReportCellErrorData(errors.map(d => ErrorViewElement(d.message, d.stackTrace)))}
}
case class ReportCellErrorData(errors:List[ErrorViewElement]) extends PageData

class ReportCellErrorsPageComponent(errors:List[ErrorViewElement], browserSize:Dimension, previousPageData:Option[PreviousPageData]) extends MigPanel("") with PageComponent {
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