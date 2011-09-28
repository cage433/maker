package starling.gui.pages

import starling.gui._
import api._
import starling.pivot._
import starling.browser.common.GuiUtils._
import java.awt.Dimension
import swing.event.{Event, ButtonClicked, SelectionChanged}
import swing._
import javax.swing.DefaultComboBoxModel
import starling.gui.utils.RichReactor
import RichReactor._
import starling.rmi.StarlingServer
import starling.daterange.Day

import starling.utils.ImplicitConversions._
import starling.gui.StarlingLocalCache._
import starling.browser.common.MigPanel
import starling.browser._
import common.RoundedBorder
import starling.fc2.api.FC2Service
import starling.reports.ReportService

class FC2Context(val service:FC2Service)

abstract class AbstractFC2PivotPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null) extends
  AbstractPivotPage(pivotPageState, edits) with FC2Page {
}

trait FC2Page extends Page {
  def bundle = "StarlingServer"
  type SC = FC2Context
  def createServerContext(sc:ServerContext) = new FC2Context(sc.lookup(classOf[FC2Service]))
}

trait FC2SubmitRequest[R] extends SubmitRequest[R] {
  def baseSubmit(serverContext:ServerContext) = {
    submit(new FC2Context(serverContext.lookup(classOf[FC2Service])))
  }
  def submit(server:FC2Context):R
}

trait FC2Bookmark extends Bookmark {
  def createFC2Page(day:Option[Day], fc2Context:FC2Context, context:PageContext):Page
  def createPage(day:Option[BrowserDay], serverContext:ServerContext, context:PageContext):Page = {
    val realDay = day.map( d => Day(d.year, d.month, d.dayOfMonth))
    createFC2Page(realDay, new FC2Context(serverContext.lookup(classOf[FC2Service])), context)
  }
}



/**
 * For viewing (and uploading?) market data.
 */
case class MarketDataPage(
        marketDataIdentifier:MarketDataPageIdentifier,
        pageState : MarketDataPageState
        ) extends AbstractPivotPage(pageState.pivotPageState, pageState.edits) {
  def this(mdi:MarketDataIdentifier, pageState : MarketDataPageState) = this(StandardMarketDataPageIdentifier(mdi), pageState)


  def bundle = "StarlingServer"

  type SC = ServerContext

  def createServerContext(sc: ServerContext) = sc

  def text = "Market Data Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_market_data.png")

  def selfPage(pivotPageState: PivotPageState, edits:PivotEdits) = new MarketDataPage(marketDataIdentifier, MarketDataPageState(pivotPageState, pageState.marketDataType, edits))

  def dataRequest(serverContext:ServerContext) = {
    val fc2Service = serverContext.lookup(classOf[FC2Service])
    fc2Service.readAllMarketData(marketDataIdentifier, pageState.marketDataType, pageState.edits, pageState.pivotPageState.pivotFieldParams)
  }

  override def save(serverContext:ServerContext, edits:PivotEdits) = {
    val fc2Service = serverContext.lookup(classOf[FC2Service])
    fc2Service.saveMarketData(marketDataIdentifier, pageState.marketDataType, edits)
  }

  override def postSave(i:Int, context:PageContext) {
    marketDataIdentifier match {
      case StandardMarketDataPageIdentifier(mi) => {
        context.localCache.latestMarketDataVersionIfValid(mi.selection) match {
          case Some(v) => {
            val maxVersion = math.max(i, v)
            val newPage = copy(marketDataIdentifier=StandardMarketDataPageIdentifier(mi.copyVersion(maxVersion)), pageState = pageState.copy(edits = PivotEdits.Null))
            context.goTo(newPage)
          }
          case None =>
        }
      }
    }
  }

  override def latestPage(localCache:LocalCache) = {
    marketDataIdentifier match {
      case StandardMarketDataPageIdentifier(mi) => {
        localCache.latestMarketDataVersionIfValid(mi.selection) match {
          case Some(v) => {
            // TODO - All edits are being removed - not just the ones that are cancelled out by the update.
            copy(marketDataIdentifier=StandardMarketDataPageIdentifier(mi.copyVersion(v)), pageState = pageState.copy(edits = PivotEdits.Null))
          }
          case _ => this
        }
      }
      case _ => this
    }
  }

  override def subClassesPageData(serverContext:ServerContext) = {
    val fc2Service = serverContext.lookup(classOf[FC2Service])
    val avaliableMarketDataTypes = fc2Service.marketDataTypeLabels(marketDataIdentifier)
    val selected = pageState.marketDataType match {
      case Some(mdt) => Some(mdt)
      case None => avaliableMarketDataTypes.headOption
    }
    Some(MarketDataPagePageData(avaliableMarketDataTypes, selected))
  }

  override def bookmark(serverContext:ServerContext, pd:PageData):Bookmark = {
    val starlingServer = serverContext.lookup(classOf[ReportService])

    val singleObservationDay = pd match {
      case PivotTablePageData(pivotData,_) => {
        pivotData.pivotFieldsState.fieldSelection(Field("Observation Day")) match {
          case Some(s) if s.size == 1 => Some(s.head)
          case _ => None
        }
      }
      case _ => None
    }
    
    if (singleObservationDay.isDefined && marketDataIdentifier.isCurrent) {
      val pfs = pd.asInstanceOf[PivotTablePageData].pivotData.pivotFieldsState
      val newPivotFieldState = pfs.removeFilter(Field("Observation Day"))
      val newPivotPageState = pageState.pivotPageState.copyPivotFieldsState(newPivotFieldState)
      val newPageState = pageState.copy(pivotPageState = newPivotPageState)
      marketDataIdentifier match {
        case x:StandardMarketDataPageIdentifier => MarketDataBookmark(marketDataIdentifier.selection, newPageState)
        case x:ReportMarketDataPageIdentifier if x.reportParameters.curveIdentifier.tradesUpToDay == singleObservationDay.get => {
          ReportMarketDataBookmark(marketDataIdentifier.selection, newPageState, starlingServer.createUserReport(x.reportParameters))
        }
        case _ => PageBookmark(this)
      }
    } else {
      PageBookmark(this)
    }
  }

  override def configPanel(context:PageContext, pageData:PageData, tableSelection:() => TableSelection) = {
    val data = pageData match {case v:PivotTablePageData => v.subClassesPageData match {case Some(d:MarketDataPagePageData) => d}}

    //Save the layout as the default for use the next time this market data type is selected
    (pageState.marketDataType, pageState.pivotPageState.pivotFieldParams.pivotFieldState) match {
      case (Some(mdt), Some(pfs)) => context.putSetting(StandardUserSettingKeys.UserMarketDataTypeLayout,
        context.getSetting(StandardUserSettingKeys.UserMarketDataTypeLayout, Map[MarketDataTypeLabel,PivotFieldsState]()) +
                (mdt -> pfs)
      )
      case _ =>
    }

    val configPanel = new MigPanel with ConfigPanel {
      def displayName = "Market Data Selection"

      val pricingGroupPanel = new MigPanel {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        private val importButton = new Button {
          val observationDay = Day.today.previousWeekday
          enabled = !marketDataIdentifier.selection.isNull
          tooltip = "Import and snapshot market data for previous weekday"
          icon = StarlingIcons.icon("/icons/14x14_download_data.png")

          reactions += {
            case ButtonClicked(_) => {
              val day = observationDay
              context.submit(SnapshotSubmitRequest(marketDataIdentifier.selection, day.asInstanceOf[Day]))
            }
          }
        }

        val pricingGroupSelector = new MarketDataSelectionComponent(context, None, marketDataIdentifier.selection)

        val snapshotsComboBoxModel = new DefaultComboBoxModel
        val snapshotsComboBox = new ComboBox[SnapshotComboValue](List(SnapshotComboValue(None))) { // Got to pass a list in - not very good but we remove it straight away.
          peer.setModel(snapshotsComboBoxModel)
          snapshotsComboBoxModel.removeAllElements()

          {val snapshots = context.localCache.snapshots(None).getOrElse(marketDataIdentifier.selection, List())
            SnapshotComboValue(None) :: snapshots.map(ss=>SnapshotComboValue(Some(ss))).toList}.foreach(snapshotsComboBoxModel.addElement(_))

          def initialize() {
            selection.item = {
              val mdi = marketDataIdentifier.marketDataIdentifier
              mdi.marketDataVersion match {
                case SnapshotMarketDataVersion(ss) => SnapshotComboValue(Some(ss))
                case SpecificMarketDataVersion(version) => SnapshotComboValue(None)
              }
            }
          }
          initialize()
          def value = {
            selection.item match {
              case SnapshotComboValue(Some(ss)) => SnapshotMarketDataVersion(ss)
              case SnapshotComboValue(None) => SpecificMarketDataVersion(context.localCache.latestMarketDataVersion(marketDataIdentifier.selection))
            }
          }
        }
        val snapshotsPanel = new MigPanel("insets 0") {
          add(new Label("as of:") {tooltip = "Market data as of selected date or current"})
          add(snapshotsComboBox)
        }

        add(importButton)
        add(pricingGroupSelector)
        add(snapshotsPanel)
      }

      val extraPanel = new MigPanel("hidemode 3") {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        val typeLabel = new Label("Type:")

        private val labels = if (data.marketDataTypeLabels.isEmpty) List(MarketDataTypeLabel("")) else data.marketDataTypeLabels
        val dataTypeCombo = new ComboBox(labels) {
          renderer = ListView.Renderer(_.toString)
          data.selection match {
            case Some(mdt) => selection.item = mdt
            case None =>
          }
          enabled = data.marketDataTypeLabels.nonEmpty
          minimumSize = new Dimension(100, preferredSize.height)
        }

        val filterDataCheckbox = new CheckBox("Filter Market Data For Report") {
          if (marketDataIdentifier.filteredMarketData) {
            selected = true
            visible = true
          } else {
            visible = false
          }
        }

        add(typeLabel)
        add(dataTypeCombo)
        add(filterDataCheckbox, "gapleft unrel")
      }

      context.remotePublisher.reactions += {
        case mdss: MarketDataSnapshotSet => if (marketDataIdentifier.selection == mdss.selection) {
          this.suppressing(pricingGroupPanel.snapshotsComboBox.selection) {
            pricingGroupPanel.snapshotsComboBoxModel.addElement(SnapshotComboValue(Some(mdss.newSnapshot)))
          }
        }
      }

      add(pricingGroupPanel, "wrap")
      add(extraPanel)

      override def revert() {
        this.suppressing(extraPanel.dataTypeCombo.selection, pricingGroupPanel.pricingGroupSelector, pricingGroupPanel.snapshotsComboBox.selection, extraPanel.filterDataCheckbox) {
          data.selection match {
            case Some(mdt) => extraPanel.dataTypeCombo.selection.item = mdt
            case None =>
          }
          pricingGroupPanel.pricingGroupSelector.selection = marketDataIdentifier.selection
          pricingGroupPanel.snapshotsComboBox.initialize()
          extraPanel.filterDataCheckbox.selected = marketDataIdentifier.filteredMarketData
        }
      }

      reactions += {
        case SelectionChanged(extraPanel.dataTypeCombo) => {
          val days = pageState.pivotPageState.pivotFieldParams.pivotFieldState.flatMap {
            _.fieldSelection(Field("Observation Day")).asInstanceOf[Option[Set[Day]]]
          }

          MarketDataPage.goTo(context, marketDataIdentifier, Some(extraPanel.dataTypeCombo.selection.item), days)
        }
        case SelectionChanged(pricingGroupPanel.snapshotsComboBox) =>
          context.goTo(copy(marketDataIdentifier=StandardMarketDataPageIdentifier(marketDataIdentifier.marketDataIdentifier.copy(marketDataVersion = pricingGroupPanel.snapshotsComboBox.value))))
        case MarketDataSelectionChanged(selection) => context.goTo(
          copy(marketDataIdentifier=StandardMarketDataPageIdentifier(MarketDataIdentifier(selection, context.localCache.latestMarketDataVersion(selection))))
        )
        case ButtonClicked(b) => {context.goTo(copy(marketDataIdentifier = StandardMarketDataPageIdentifier(marketDataIdentifier.marketDataIdentifier)))}
      }
      listenTo(extraPanel.dataTypeCombo.selection, pricingGroupPanel.pricingGroupSelector, pricingGroupPanel.snapshotsComboBox.selection, extraPanel.filterDataCheckbox)
    }

    Some(ConfigPanels(List(configPanel), new Label(""), Action("BLA"){}))
  }
}

case class ReportMarketDataBookmark(selection:MarketDataSelection, pageState:MarketDataPageState,
                                    userReportData:UserReportData) extends StarlingBookmark {
  def daySensitive = true
  def createStarlingPage(day:Option[Day], serverContext:StarlingServerContext, context:PageContext) = {
    val newPFS = pageState.pivotPageState.pivotFieldParams.pivotFieldState.map(pfs => {
      pfs.addFilter((Field("Observation Day"), Set(day.get)))
    })
    val newPivotPageState = pageState.pivotPageState.copyPivotFieldsState(newPFS)
    val newSelection = ReportMarketDataPageIdentifier(serverContext.reportService.createReportParameters(userReportData, day.get))
    MarketDataPage(newSelection, pageState.copy(pivotPageState = newPivotPageState))
  }
}

case class MarketDataBookmark(selection:MarketDataSelection, pageState:MarketDataPageState) extends FC2Bookmark {
  def daySensitive = true
  def createFC2Page(day:Option[Day], serverContext:FC2Context, context:PageContext) = {
    val newPFS = pageState.pivotPageState.pivotFieldParams.pivotFieldState.map(pfs => {
      pfs.addFilter((Field("Observation Day"), Set(day.get)))
    })
    val newPivotPageState = pageState.pivotPageState.copyPivotFieldsState(newPFS)
    val newSelection = StandardMarketDataPageIdentifier(serverContext.service.latestMarketDataIdentifier(selection))
    MarketDataPage(newSelection, pageState.copy(pivotPageState = newPivotPageState))
  }
}

object MarketDataPage {
  //Goes to the MarketDataPage and picks the default market data type (if not specified) and pivot field state
  def pageFactory(
            pageContext:PageContext,
            marketDataIdentifier:MarketDataPageIdentifier,
            marketDataType:Option[MarketDataTypeLabel],
            observationDays:Option[Set[Day]]): ServerContext=>Page = {
    (serverContext) => {
      val mdt = marketDataType.orElse(serverContext.lookup(classOf[FC2Service]).marketDataTypeLabels(marketDataIdentifier).headOption)
      val fs = pageContext.getSetting(
        StandardUserSettingKeys.UserMarketDataTypeLayout, Map[MarketDataTypeLabel, PivotFieldsState]()
      ).get(mdt)

      val fieldsState = (fs, observationDays) match {
        case (Some(fs0), Some(days)) => Some(fs0.addFilter(Field("Observation Day") → days.asInstanceOf[Set[Any]]))
        case _ => fs
      }

      new MarketDataPage(marketDataIdentifier, MarketDataPageState(
        marketDataType = mdt,
        pivotPageState = PivotPageState(false, PivotFieldParams(true, fieldsState))
      ))
    }
  }
  def goTo(
            pageContext:PageContext,
            marketDataIdentifier:MarketDataPageIdentifier,
            marketDataType:Option[MarketDataTypeLabel],
            observationDays:Option[Set[Day]],
            modifiers:Modifiers=Modifiers.None) {
    pageContext.createAndGoTo( (sc) => pageFactory(pageContext, marketDataIdentifier, marketDataType, observationDays)(sc), modifiers= modifiers)
  }
}

case class MarketDataSelectionChanged(selection:MarketDataSelection) extends Event

object MarketDataSelectionComponent {
  def storeMarketDataSelection(pageContext:PageContext, selection:MarketDataSelection) {
    pageContext.putSetting(StandardUserSettingKeys.InitialMarketDataSelection, selection)
    selection.pricingGroup.map(pg => pageContext.putSetting(StandardUserSettingKeys.PricingGroupDefault, pg))
    selection.excel.map(excel => pageContext.putSetting(StandardUserSettingKeys.ExcelMarketDataDefault, excel))
  }
}

class MarketDataSelectionComponent(pageContext:PageContext, maybeDesk:Option[Desk],
                                   marketDataSelection:MarketDataSelection,
                                   orientation:scala.swing.Orientation.Value=scala.swing.Orientation.Horizontal)
        extends MigPanel("insets 0") with Revertable {


  def revert() {this.suppressingSelf(selection = marketDataSelection)}

  private val minWidthForComboBox = 200

  private val pricingGroups = pageContext.localCache.pricingGroups(maybeDesk)
  private val pricingGroupCheckBox = new CheckBox {
    text = "Pricing Group:"
    selected = marketDataSelection.pricingGroup.isDefined
    enabled = !pricingGroups.isEmpty
  }

  private val pricingGroupCombo = if (pricingGroups.isEmpty) {
    new ComboBox(List(PricingGroup(""))) {
      enabled=false
      minimumSize = new Dimension(minWidthForComboBox, minimumSize.height)
    }
  } else {
    new ComboBox(pricingGroups) {
      minimumSize = new Dimension(minWidthForComboBox, minimumSize.height)
      marketDataSelection.pricingGroup match {
        case Some(pg) => selection.item = pg
        case None => {
          pageContext.getSettingOption(StandardUserSettingKeys.PricingGroupDefault) match {
            case Some(pg) => selection.item = pg
            case None =>
          }
        }
      }
      enabled = marketDataSelection.pricingGroup.isDefined
    }
  }

  private val excelCheckBox = new CheckBox {
    text = "Excel Market Data:"
    enabled = !pageContext.localCache.excelDataSets.isEmpty
    selected = marketDataSelection.excel.isDefined && enabled
  }
  private val excelCombo : ComboBox[String] = createExcelCombo(pageContext.localCache.excelDataSets)

  private def createExcelCombo(values:List[String]) = {
    if (pageContext.localCache.excelDataSets.isEmpty) {
      new ComboBox(List("")) {
        enabled = false
        minimumSize = new Dimension(minWidthForComboBox, minimumSize.height)
      }
    } else {
      new ComboBox(values) {
        minimumSize = new Dimension(minWidthForComboBox, minimumSize.height)
        if (marketDataSelection.excel.isDefined) {
          selection.item = marketDataSelection.excel.get
        } else {
          pageContext.getSettingOption(StandardUserSettingKeys.ExcelMarketDataDefault) match {
            case Some(excel) => selection.item = excel
            case None =>
          }
        }
        enabled = marketDataSelection.excel.isDefined
      }
    }
  }

  def selection_= (se:MarketDataSelection) {
    se.pricingGroup match {
      case Some(pg) => {
        pricingGroupCheckBox.selected = true
        pricingGroupCombo.selection.item = pg
        pricingGroupCombo.enabled = true
      }
      case None => {
        pricingGroupCheckBox.selected = false
        pricingGroupCombo.enabled = false
      }
    }
    se.excel match {
      case Some(name) => {
        excelCheckBox.selected = true
        excelCombo.selection.item = name
        excelCombo.enabled = true
      }
      case None => {
        excelCheckBox.selected = false
        excelCombo.enabled = false
      }
    }
  }

  def selection = MarketDataSelection(
    if (pricingGroupCheckBox.selected) Some(pricingGroupCombo.selection.item.asInstanceOf[PricingGroup]) else None,
    if (excelCheckBox.selected) Some(excelCombo.selection.item) else None
  )

  def fireNewSelection() {
    val se = selection
    MarketDataSelectionComponent.storeMarketDataSelection(pageContext, se)
    publish(MarketDataSelectionChanged(se))
  }

  listenTo(pageContext.remotePublisher)
  reactions += {
    case ExcelMarketListUpdate(values) => {
      val currentSelection = excelCombo.selection.item
      excelCheckBox.enabled = true
      this.suppressing(excelCombo.selection) {
        excelCombo.peer.setModel(ComboBox.newConstantModel(values))
        if (values.contains(currentSelection)) {
          excelCombo.selection.item = currentSelection
        }
      }
    }
  }

  reactions += {
    case ButtonClicked(`pricingGroupCheckBox`) => { pricingGroupCombo.enabled = pricingGroupCheckBox.selected; fireNewSelection() }
    case SelectionChanged(`pricingGroupCombo`) => fireNewSelection()
    case ButtonClicked(`excelCheckBox`) => { excelCombo.enabled = excelCheckBox.selected; fireNewSelection() }
    case SelectionChanged(`excelCombo`) => fireNewSelection()
  }

  listenTo(pricingGroupCheckBox, pricingGroupCombo.selection, excelCheckBox, excelCombo.selection)

  val layoutInfo = orientation match {
    case scala.swing.Orientation.Vertical => "wrap"
    case scala.swing.Orientation.Horizontal => "gapright rel"
  }
  add(pricingGroupCheckBox)
  add(pricingGroupCombo, layoutInfo)
  add(excelCheckBox, excelCombo)
}

case class SnapshotComboValue(maybeSnapshot:Option[SnapshotIDLabel]) {
  override def toString = maybeSnapshot match {
    case Some(ss) => ss.shortString
    case None => "Current"
  }
}

case class MarketDataPageState(
        pivotPageState : PivotPageState = PivotPageState(false, PivotFieldParams(true, None)),
        marketDataType : Option[MarketDataTypeLabel] = None,
        edits          : PivotEdits = PivotEdits.Null)

case class MarketDataPagePageData(marketDataTypeLabels:List[MarketDataTypeLabel], selection:Option[MarketDataTypeLabel]) extends PageData
