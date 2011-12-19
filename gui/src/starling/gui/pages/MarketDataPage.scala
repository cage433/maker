package starling.gui.pages

import starling.gui._
import api._
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
import starling.browser._
import common.{ResizingComboBox, MigPanel, RoundedBorder}
import starling.fc2.api.FC2Facility
import starling.reports.facility.ReportFacility
import starling.pivot.model.UndefinedValue
import starling.pivot.Field._
import starling.pivot._
import starling.gui.osgi.StarlingBrowserBundle


class FC2Context(val service:FC2Facility)

abstract class AbstractFC2PivotPage(pivotPageState:PivotPageState, edits:PivotEdits=PivotEdits.Null) extends
  AbstractPivotPage(pivotPageState, edits) with FC2Page {
}

trait FC2Page extends Page {
  def bundle = StarlingBrowserBundle.BundleName
  type SC = FC2Context
  def createServerContext(sc:ServerContext) = new FC2Context(sc.lookup(classOf[FC2Facility]))
}

trait FC2SubmitRequest[R] extends SubmitRequest[R] {
  def baseSubmit(serverContext:ServerContext) = {
    submit(new FC2Context(serverContext.lookup(classOf[FC2Facility])))
  }
  def submit(server:FC2Context):R
}

trait FC2Bookmark extends Bookmark {
  def createFC2Page(day:Option[Day], fc2Context:FC2Context, context:PageContext):Page
  def createPage(day:Option[BrowserDay], serverContext:ServerContext, context:PageContext):Page = {
    val realDay = day.map( d => Day(d.year, d.month, d.dayOfMonth))
    createFC2Page(realDay, new FC2Context(serverContext.lookup(classOf[FC2Facility])), context)
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


  def bundle = StarlingBrowserBundle.BundleName

  type SC = ServerContext

  def createServerContext(sc: ServerContext) = sc

  def text = "Market Data Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_market_data.png")

  def selfPage(pivotPageState: PivotPageState, edits:PivotEdits) = new MarketDataPage(marketDataIdentifier, MarketDataPageState(pivotPageState, pageState.marketDataType, edits))

  def dataRequest(serverContext:ServerContext) = {
    val fc2Service = serverContext.lookup(classOf[FC2Facility])
    fc2Service.readAllMarketData(marketDataIdentifier, pageState.marketDataType, pageState.edits, pageState.pivotPageState.pivotFieldParams)
  }

  override def save(serverContext:ServerContext, edits:PivotEdits) = {
    val fc2Service = serverContext.lookup(classOf[FC2Facility])
    fc2Service.saveMarketData(marketDataIdentifier, pageState.marketDataType, edits)
  }

  override def postSave(i:Int, context:PageContext) {
    marketDataIdentifier match {
      case mdpi:MarketDataPageIdentifier => {
        context.localCache.latestMarketDataVersionIfValid(mdpi.selection) match {
          case Some(v) => {
            val maxVersion = math.max(i, v)
            val newPage = copy(marketDataIdentifier=StandardMarketDataPageIdentifier(MarketDataIdentifier(mdpi.selection, maxVersion)), pageState = pageState.copy(edits = PivotEdits.Null))
            context.goTo(newPage)
          }
          case None =>
        }
      }
    }
  }

  override def latestPage(localCache:LocalCache) = {
    marketDataIdentifier match {
      case StandardMarketDataPageIdentifier(mi@MarketDataIdentifier(_,SpecificMarketDataVersion(_))) => {
        localCache.latestMarketDataVersionIfValid(mi.selection) match {
          case Some(v) => copy(marketDataIdentifier=StandardMarketDataPageIdentifier(mi.copyVersion(v)))
          case _ => this
        }
      }
      case _ => this
    }
  }

  override def bookmark(serverContext:ServerContext, pd:PageData):Bookmark = {
    val starlingServer = serverContext.lookup(classOf[ReportFacility])
    val singleObservationDay = pd match {
      case PivotTablePageData(pivotData,_) => {
        pivotData.pivotFieldsState.fieldSelection(Field("Observation Day")) match {
          case Some(s) if s.size == 1 && s.contains(UndefinedValue) => None
          case Some(s) if s.size == 1 && s != Set(LabeledFilterSelection("Latest")) => Some(s.head)
          case Some(s) if s.size == 2 && s.contains(UndefinedValue) => Some(s.filterNot(_ == UndefinedValue))
          case _ => None
        }
      }
      case _ => None
    }
    
    if (singleObservationDay.isDefined && marketDataIdentifier.isCurrent) {
      marketDataIdentifier match {
        case x:StandardMarketDataPageIdentifier => MarketDataBookmark(marketDataIdentifier.selection, pageState)
        case x:ReportMarketDataPageIdentifier if x.reportParameters.curveIdentifier.observationDayAndTime == singleObservationDay.get => {
          ReportMarketDataBookmark(marketDataIdentifier.selection, pageState, starlingServer.createUserReport(x.reportParameters))
        }
        case _ => PageBookmark(this)
      }
    } else {
      PageBookmark(this)
    }
  }

  override def configPanel(context:PageContext, pageData:PageData, tableSelection:() => TableSelection) = {
    val availableMarketDataTypes = context.localCache.marketDataTypes(marketDataIdentifier.selection)
    val selectedMarketDataType = pageState.marketDataType.orElse(if(marketDataIdentifier.selection.isNull) None else Some(MarketDataTypeLabel.Default))

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
          tooltip = "Import market data for previous weekday"
          icon = StarlingIcons.icon("/icons/14x14_download_data.png")

          reactions += {
            case ButtonClicked(_) => {
              val day = observationDay
              context.submit(ImportMarketDataRequest(marketDataIdentifier.selection, day.asInstanceOf[Day]))
            }
          }
        }

        val pricingGroupSelector = new MarketDataSelectionComponent(context, None,
          marketDataIdentifier.marketDataIdentifier, scala.swing.Orientation.Vertical, true)

        private val snapshotButton = new Button {
          enabled = !marketDataIdentifier.selection.isNull
          tooltip = "Snapshot this market data"
          icon = StarlingIcons.icon("/icons/14x14_camera_lens.png")
          reactions += {
            case ButtonClicked(_) => {
              context.submit(SnapshotMarketDataRequest(marketDataIdentifier.marketDataIdentifier))
            }
          }
        }

        add(pricingGroupSelector, "grow, spany")
        add(importButton, "ay top, wrap")
        add(snapshotButton, "skip 1, ay bottom")
      }

      val extraPanel = new MigPanel("hidemode 3") {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        val typeLabel = new Label("Type:")

        private val labels = if (availableMarketDataTypes.isEmpty) List(MarketDataTypeLabel("")) else availableMarketDataTypes
        val dataTypeCombo = new ComboBox(labels) {
          renderer = ListView.Renderer(_.toString)
          selectedMarketDataType match {
            case Some(mdt) => selection.item = mdt
            case None =>
          }
          enabled = availableMarketDataTypes.nonEmpty
          minimumSize = new Dimension(100, preferredSize.height)
        }

        add(typeLabel)
        add(dataTypeCombo, "pushx, grow")
      }

      val extraPanel2 = new MigPanel {
        border = RoundedBorder(colour = PivotTableBackgroundColour)
        visible = marketDataIdentifier.filteredMarketData
        val filterDataCheckbox = new CheckBox("Filter Market Data For Report") {
          if (marketDataIdentifier.filteredMarketData) {
            selected = true
            visible = true
          } else {
            visible = false
          }
        }

        add(filterDataCheckbox)
      }

      add(pricingGroupPanel)
      add(extraPanel, "ay top, growx, split, spany 2, flowy")
      add(extraPanel2, "ay top, growx, hidemode 3")

      override def revert() {
        this.suppressing(extraPanel.dataTypeCombo.selection, pricingGroupPanel.pricingGroupSelector, extraPanel2.filterDataCheckbox) {
          selectedMarketDataType match {
            case Some(mdt) => extraPanel.dataTypeCombo.selection.item = mdt
            case None =>
          }
          pricingGroupPanel.pricingGroupSelector.revert()
          extraPanel2.filterDataCheckbox.selected = marketDataIdentifier.filteredMarketData
        }
      }

      reactions += {
        case SelectionChanged(extraPanel.dataTypeCombo) => {
          MarketDataPage.goTo(context, marketDataIdentifier, Some(extraPanel.dataTypeCombo.selection.item), observationDays)
        }
        case MarketDataSelectionChanged(mdi0) => {
          val mdt = if (extraPanel.dataTypeCombo.enabled && extraPanel.dataTypeCombo.selection.item.name.nonEmpty) {
            Some(extraPanel.dataTypeCombo.selection.item)
          } else {
            None
          }
          MarketDataPage.goTo(context,
            StandardMarketDataPageIdentifier(mdi0),
            mdt, observationDays
          )
        }
        case ButtonClicked(extraPanel2.filterDataCheckbox) => {context.goTo(copy(marketDataIdentifier = StandardMarketDataPageIdentifier(marketDataIdentifier.marketDataIdentifier), pageState = pageState.copy(edits = PivotEdits.Null)))}
      }
      listenTo(extraPanel.dataTypeCombo.selection, pricingGroupPanel.pricingGroupSelector, extraPanel2.filterDataCheckbox)
    }

    Some(ConfigPanels(List(configPanel), new Label(""), Action("BLA"){}))
  }

  private def observationDays: Option[Set[Day]] = pageState.pivotPageState.pivotFieldParams.pivotFieldState.flatMap {
    _.fieldSelection(Field("Observation Day")).asInstanceOf[Option[Set[Day]]]
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
    val ObDay = Field("Observation Day")
    val newPFS = pageState.pivotPageState.pivotFieldParams.pivotFieldState.map(pfs => {
      pfs.mapFilters {
        case c@(ObDay, selection) => selection match {
          case SomeSelection(v) if v.size == 1 => ObDay -> SomeSelection(Set(day.get))
          case SomeSelection(v) if v.size == 2 && v.contains(UndefinedValue) => ObDay -> SomeSelection(Set(UndefinedValue, day.get))
          case _ => ObDay -> selection
        }
        case o => o
      }
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
      val mdt = if (marketDataIdentifier.selection.isNull) None else marketDataType.orElse(Some(MarketDataTypeLabel.Default))
      val fs = pageContext.getSetting(
        StandardUserSettingKeys.UserMarketDataTypeLayout, Map[MarketDataTypeLabel, PivotFieldsState]()
      ).get(mdt)

      val fieldsState = (fs, observationDays) match {
        case (Some(fs0), Some(days)) => Some(fs0.addFilter(Field("Observation Day") → days.asInstanceOf[Set[Any]]))
        case _ => fs
      }

      new MarketDataPage(marketDataIdentifier, MarketDataPageState(
        marketDataType = mdt,
        pivotPageState = PivotPageState(false, PivotFieldParams(true, fieldsState), OtherLayoutInfo(totals = Totals.Null, frozen = false))
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

case class MarketDataSelectionChanged(selection:MarketDataIdentifier) extends Event

object MarketDataSelectionComponent {
  def storeMarketDataSelection(pageContext:PageContext, selection:MarketDataSelection) {
    pageContext.putSetting(StandardUserSettingKeys.InitialMarketDataSelection, selection)
    selection.pricingGroup.map(pg => pageContext.putSetting(StandardUserSettingKeys.PricingGroupDefault, pg))
    selection.excel.map(excel => pageContext.putSetting(StandardUserSettingKeys.ExcelMarketDataDefault, excel))
  }
}

class MarketDataSelectionComponent(context:PageContext, maybeDesk:Option[Desk],
                                   marketDataIdentifier:MarketDataIdentifier,
                                   orientation:scala.swing.Orientation.Value=scala.swing.Orientation.Horizontal,
                                   showShapshotChooser:Boolean=false)
        extends MigPanel("insets 0") with Revertable {

  private val marketDataSelection = marketDataIdentifier.selection

  def revert() {
    this.suppressingSelf(selection = marketDataIdentifier)
    updateStateOfComponents()
  }

  private val pricingGroups = context.localCache.pricingGroups(maybeDesk)
  private val pricingGroupCheckBox = new CheckBox {
    text = "Pricing Group:"
    selected = marketDataSelection.pricingGroup.isDefined
    enabled = !pricingGroups.isEmpty
  }

  private val pricingGroupCombo = if (pricingGroups.isEmpty) {
    new ComboBox(List(PricingGroup("")))
  } else {
    new ResizingComboBox(pricingGroups) {
      marketDataSelection.pricingGroup match {
        case Some(pg) => selection.item = pg
        case None => {
          context.getSettingOption(StandardUserSettingKeys.PricingGroupDefault) match {
            case Some(pg) => selection.item = pg
            case None =>
          }
        }
      }
    }
  }

  private val excelCheckBox = new CheckBox {
    text = "Excel Market Data:"
    enabled = !context.localCache.excelDataSets.isEmpty
    selected = marketDataSelection.excel.isDefined && enabled
  }
  private val excelCombo : ComboBox[String] = createExcelCombo(context.localCache.excelDataSets)

  private def createExcelCombo(values:List[String]) = {
    if (context.localCache.excelDataSets.isEmpty) {
      new ComboBox(List(""))
    } else {
      new ResizingComboBox(values) {
        if (marketDataSelection.excel.isDefined) {
          selection.item = marketDataSelection.excel.get
        } else {
          context.getSettingOption(StandardUserSettingKeys.ExcelMarketDataDefault) match {
            case Some(excel) => selection.item = excel
            case None =>
          }
        }
        enabled = marketDataSelection.excel.isDefined
      }
    }
  }

  private val snapshotsComboBoxModel = new DefaultComboBoxModel
  private val snapshotsComboBox = new ResizingComboBox[SnapshotComboValue](List(SnapshotComboValue(None)), Some(snapshotsComboBoxModel)) { // Got to pass a list in - not very good but we remove it straight away.
    snapshotsComboBoxModel.removeAllElements()

    {val snapshots = context.localCache.snapshots(None).getOrElse(marketDataIdentifier.selection, List())
      val snapshotComboValues = SnapshotComboValue(None) :: snapshots.map(ss=>SnapshotComboValue(Some(ss))).toList
      snapshotComboValues.foreach(snapshotsComboBoxModel.addElement(_))}

    def initialize(mdv:MarketDataVersion) {
      selection.item = {
        mdv match {
          case SnapshotMarketDataVersion(ss) => SnapshotComboValue(Some(ss))
          case SpecificMarketDataVersion(version) => SnapshotComboValue(None)
        }
      }
    }

    initialize(marketDataIdentifier.marketDataVersion)

    def valueForSelection(mds:MarketDataSelection) = {
      selection.item match {
        case SnapshotComboValue(Some(ss)) => SnapshotMarketDataVersion(ss)
        case SnapshotComboValue(None) => SpecificMarketDataVersion(context.localCache.latestMarketDataVersion(mds))
      }
    }
  }

  def selection_=(mdi:MarketDataIdentifier) {
    val se = mdi.selection
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
    snapshotsComboBox.initialize(mdi.marketDataVersion)
  }

  def selection = {
    val mds = MarketDataSelection(
      if (pricingGroupCheckBox.selected) Some(pricingGroupCombo.selection.item.asInstanceOf[PricingGroup]) else None,
      if (excelCheckBox.selected) Some(excelCombo.selection.item) else None
    )
    MarketDataIdentifier(mds, snapshotsComboBox.valueForSelection(mds))
  }

  def fireNewSelection() {
    val mdi = selection
    MarketDataSelectionComponent.storeMarketDataSelection(context, mdi.selection)
    publish(MarketDataSelectionChanged(mdi))
  }

  reactions += {
    case MarketDataSnapshot(snapshotID) if (snapshotID.marketDataSelection == marketDataIdentifier.selection) => {
      snapshotsComboBoxModel.insertElementAt(SnapshotComboValue(Some(snapshotID)), 1)
    }
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
  listenTo(context.remotePublisher)

  private def updateStateOfComponents() {
    pricingGroupCombo.enabled = pricingGroupCheckBox.selected && !pricingGroups.isEmpty
    excelCombo.enabled = excelCheckBox.selected && !context.localCache.excelDataSets.isEmpty
    snapshotsComboBox.enabled = pricingGroupCombo.enabled && !excelCombo.enabled
  }

  reactions += {
    case ButtonClicked(`pricingGroupCheckBox`) => { updateStateOfComponents(); fireNewSelection() }
    case SelectionChanged(`pricingGroupCombo`) => fireNewSelection()
    case ButtonClicked(`excelCheckBox`) => { updateStateOfComponents(); fireNewSelection() }
    case SelectionChanged(`excelCombo`) => fireNewSelection()
    case SelectionChanged(`snapshotsComboBox`) => fireNewSelection()
  }

  listenTo(pricingGroupCheckBox, pricingGroupCombo.selection, excelCheckBox, excelCombo.selection, snapshotsComboBox.selection)

  val layoutInfo = orientation match {
    case scala.swing.Orientation.Vertical => "wmin 50lp, pushx, growx, wrap"
    case scala.swing.Orientation.Horizontal => "wmin 50lp, growx, gapright rel"
  }
  add(pricingGroupCheckBox)
  add(pricingGroupCombo, layoutInfo)
  add(excelCheckBox)
  val excelLayoutInfo = orientation match {
    case scala.swing.Orientation.Vertical if showShapshotChooser => "wmin 50lp, pushx, growx, wrap"
    case scala.swing.Orientation.Horizontal if showShapshotChooser => "wmin 50lp, pushx, growx, gapright rel"
    case _ => "wmin 50lp, pushx, growx"
  }
  add(excelCombo, excelLayoutInfo)
  if (showShapshotChooser) {
    val snapshotLayoutInfo = orientation match {
      case scala.swing.Orientation.Vertical => "gapleft unrel, split, spanx"
      case scala.swing.Orientation.Horizontal => ""
    }
    add(new Label("as of:"), snapshotLayoutInfo)
    add(snapshotsComboBox, "wmin 50lp, growx")
  }
  updateStateOfComponents()
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
