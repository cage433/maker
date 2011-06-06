package starling.gui

import api._
import pages._
import swing._
import event.{Event, ListSelectionChanged, ButtonClicked}
import starling.pivot._
import collection.mutable.ListBuffer
import java.awt.datatransfer.StringSelection
import starling.rmi.StarlingServer
import collection.Seq
import starling.daterange.{Day}
import view.swing._
import java.awt.{Toolkit, Dimension}
import javax.swing.ImageIcon

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
        ) extends AbstractPivotPage(pivotPageState) {

  assert(tradeSelection.intradaySubgroup.isEmpty, "Difference reports don't work with Excel trades")

  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    pageBuildingContext.cachingStarlingServer.diffReportPivot(tradeSelection, curveIdentifierDm1, curveIdentifierD,
      reportOptions, expiryDay, fromTimestamp, toTimestamp, pivotPageState.pivotFieldParams)
  }

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event,Page]]
    functions.toList
  }

  def text = tradeSelection + " " + curveIdentifierD + " vs " + curveIdentifierDm1
  def selfPage(pps:PivotPageState) = copy(pivotPageState = pps)
  override def toolbarButtons(pageContext:PageContext, data:PageData) =
    PivotReportPage.toolbarButtons(
      pageContext,
      ReportParameters(tradeSelection.withDeskTimestamp(toTimestamp), curveIdentifierD, reportOptions, expiryDay),
      data,
      false,
      pivotPageState)
}

case class PivotReportTablePageData(numErrors:Int, userReportData:UserReportData) extends PageData

case class MainPivotReportPage(showParameters:Boolean, reportParameters:ReportParameters, pivotPageState:PivotPageState) extends AbstractPivotPage(pivotPageState) {
  private val shortTitle = "Risk Report"
  val text = if (showParameters) shortTitle else reportParameters.text
  override val icon = StarlingIcons.im("/icons/16x16_report.png")
  override val shortText = if (showParameters) shortTitle else reportParameters.shortText
  override def layoutType = Some(PivotLayout.ReportLayoutType)

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
              case Some(`name`) => pnlParameters.copy(curveIdentifierFrom=pnlParameters.curveIdentifierFrom.copyMarketDataVersion(SpecificMarketDataVersion(version)))
              case _ => pnlParameters
            }
          }
        }
        val curveIdentifier = reportParameters.curveIdentifier.marketDataIdentifier.selection.excel match {
          case Some(`name`) => reportParameters.curveIdentifier.copyMarketDataVersion(SpecificMarketDataVersion(version))
          case None => reportParameters.curveIdentifier
        }
        selfReportPage(reportParameters.copy(curveIdentifier=curveIdentifier, pnlParameters=pnlParameters))
      } }
    }

    //TODO: respond to PricingGroup market data changes

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
        val timestamp = trID.tradeSystem match {
          case TradeSystemLabel.Intraday => reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.get._2
          case _ => reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.get._2.timestamp
        }
        pageContext.goTo(SingleTradePage(trID, reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map(_._1),
            TradeExpiryDay(timestamp.toDay), reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map(_._1)), newTab = ctrlDown)
      }
      case None => None
    }
  }

  override def subClassesPageData(reader:PageBuildingContext):Option[PageData] = {
    Some(PivotReportTablePageData(
      reader.cachingStarlingServer.reportErrors(reportParameters).errors.size,
      reader.starlingServer.createUserReport(reportParameters)))
  }

  def dataRequest(pageBuildingContext: PageBuildingContext) = {
    pageBuildingContext.cachingStarlingServer.reportPivot(reportParameters, pivotPageState.pivotFieldParams)
  }
  def selfPage(pps:PivotPageState) = copy(pivotPageState = pps)
  def selfReportPage(rp:ReportParameters, pps:PivotPageState = pivotPageState) = copy(reportParameters = rp, pivotPageState = pps)
  override def toolbarButtons(pageContext:PageContext, data:PageData) =
    PivotReportPage.toolbarButtons(pageContext, reportParameters, data, showParameters, pivotPageState)

  override def configPanel(context:PageContext, data:PageData) = {
    if (showParameters) {
      val (pivotData, userReportData, user0, layoutTypeOption) = data match {
        case PivotTablePageData(pivData,Some(pd),layoutTypeOption) => pd match {
          case PivotReportTablePageData(_, userReportData) => {
            (pivData, Some(userReportData), context.localCache.currentUser, layoutTypeOption)
          }
          case scd => throw new Exception("Don't know how to handle this type of subclass page data: " + scd.getClass)
        }
        case PivotTablePageData(pivData,None,layoutTypeOption) => (pivData,None,context.localCache.currentUser, layoutTypeOption)
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

        val observationDaysForPricingGroup = context.localCache.populatedObservationDaysForPricingGroup
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
          runButton.enabled = (rps != reportParameters)
        }
        def updateRunButton(mds:MarketDataSelection) {
          runButton.enabled = (mds != reportParameters.curveIdentifier.marketDataIdentifier.selection)
        }

        val saveReportButton = new SaveReportButton(context, userReportData, pivotData, pivotPageState,
          showParameters, user0, layoutTypeOption.get) {
          enabled = reportParameters.runReports
        }

        add(runButton, "ay center, tag ok")
        add(saveReportButton, "ay center")

        updateRunButton(generateRPs)

        def run() {
          pivotPageState.pivotFieldParams.pivotFieldState.map { fs => {
            val otherLayoutInfo = pivotPageState.otherLayoutInfo
            context.putSetting(StandardUserSettingKeys.DefaultReportFields, (fs,otherLayoutInfo))
          }}
          update(manualConfigPanel.generateReportParams(slideConfigPanel.slideConfig))
        }
      }

      Some(ConfigPanels(List(presetReportPanel, manualConfigPanel, slideConfigPanel), runPanel, Action("runReportAction") {runPanel.run()}))
    } else {
      None
    }
  }
}

object PivotReportPage {
  def toolbarButtons(pageContext:PageContext, reportParameters:ReportParameters, data:PageData, showParameters:Boolean,
                            pivotPageState:PivotPageState) = {
    val (pivotData, numErrors) = data match {
      case PivotTablePageData(pivData,Some(pd),_) => pd match {
        case PivotReportTablePageData(numErrors, _) => {
          (pivData, numErrors)
        }
        case scd => throw new Exception("Don't know how to handle this type of subclass page data: " + scd.getClass)
      }
      case PivotTablePageData(pivData,None,_) => (pivData,0)
      case _ => throw new Exception("Don't know how to handle this type of page data")
    }
    
    val buffer:ListBuffer[ToolBarButton] = new ListBuffer()
    if (numErrors > 0) {
      buffer += new ToolBarButton {
        text = "Errors (" + numErrors + ")"
        tooltip = "Show errors that occured whilst running this report"
        icon = StarlingIcons.icon("/icons/error.png")
        reactions += {
          case ButtonClicked(b) => {
            pageContext.goTo(ReportErrorsPage(reportParameters))
          }
        }
      }
    }

    // Always add the market data button here.
    if (reportParameters.runReports) {
      buffer += new ToolBarButton {
        text = "Market Data"
        tooltip = "Show all market data used to calculate the values shown"
        icon = StarlingIcons.icon("/icons/16x16_market_data.png")
        reactions += {
          case ButtonClicked(b) => {
            pageContext.goTo(MarketDataPage(ReportMarketDataPageIdentifier(reportParameters), MarketDataPageState()))
          }
        }
      }
    }

    val availablePages = pivotData.availablePages



    buffer.toList
  }
}

case class SaveReportRequest(reportName:String, userReportData:UserReportData, pivotLayout:PivotLayout,
                             shouldSaveLayout:Boolean, shouldAssociateLayout:Boolean, showParameters:Boolean) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {
    server.saveUserReport(reportName, userReportData, showParameters)
    if (shouldSaveLayout && shouldAssociateLayout) {
      // This is the case where it is a custom layout so we want to save the layout and associate it with this report
      server.saveLayout(pivotLayout.copy(associatedReports = List(reportName)))
    } else if (shouldAssociateLayout) {
      // This is the case where the layout is already saved but we want to associate it with this report.
      server.deleteLayout(pivotLayout.layoutName)
      server.saveLayout(pivotLayout.copy(associatedReports = reportName :: pivotLayout.associatedReports))
    }
  }
}

case class DeleteReportRequest(reportName:String) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {server.deleteUserReport(reportName)}
}

case class ReportErrorsPage(reportParameters:ReportParameters) extends Page {
  val text = "Errors in " + reportParameters.text
  def createComponent(context: PageContext, data: PageData, browserSize:Dimension) = new PivotReportErrorPageComponent(context, data, browserSize)
  def build(pageBuildingContext: PageBuildingContext) = {
    val errors = pageBuildingContext.cachingStarlingServer.reportErrors(reportParameters)
    val errorsToUse = errors.errors.map(e => ErrorViewElement(e.instrumentText, e.message))
    PivotReportErrorPageData(errorsToUse)
  }
  val icon = StarlingIcons.im("/icons/error.png")
}
case class PivotReportErrorPageData(reportErrors:List[ErrorViewElement]) extends PageData


class PivotReportErrorPageComponent(pageContext:PageContext, data:PageData, browserSize:Dimension) extends MigPanel("") with PageComponent {
  val errors = data match {
    case d:PivotReportErrorPageData => d.reportErrors
  }
  val errorView = new ErrorView(errors, Some(scala.math.round(browserSize.height / 4.0f)))
  add(errorView, "push, grow")
}

case class ReportCellErrorsPage(errors:List[StackTrace]) extends Page {
  val text = "Errors"
  val icon = StarlingIcons.im("/icons/error.png")
  def createComponent(context:PageContext, data:PageData, browserSize:Dimension) = {
    val errorsToUse = data match {
      case d:ReportCellErrorData => d.errors
    }
    new ReportCellErrorsPageComponent(errorsToUse, browserSize)
  }
  def build(pageBuildingContext:PageBuildingContext) = {ReportCellErrorData(errors.map(d => ErrorViewElement(d.message, d.stackTrace)))}
}
case class ReportCellErrorData(errors:List[ErrorViewElement]) extends PageData

class ReportCellErrorsPageComponent(errors:List[ErrorViewElement], browserSize:Dimension) extends MigPanel("") with PageComponent {
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
