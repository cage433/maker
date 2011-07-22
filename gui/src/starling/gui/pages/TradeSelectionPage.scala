package starling.gui.pages

import starling.gui._
import api._
import starling.rmi.StarlingServer
import starling.gui.GuiUtils._
import swing._
import event.{Event, KeyPressed, ButtonClicked, SelectionChanged}
import java.awt.{Dimension, Color}
import starling.pivot.view.swing._
import starling.tradestore.TradePredicate
import starling.pivot._
import collection.Seq
import collection.mutable.ListBuffer
import controller.TreePivotFilter
import javax.swing.DefaultComboBoxModel
import starling.gui.custom._
import utils.{RichReactor, RichCheckBox}
import RichCheckBox._
import RichReactor._
import starling.daterange.{Timestamp, Day}

/**
 * Page that allows you to select trades.
 */
case class TradeSelectionPage(
        tpp:TradePageParameters,
        pivotPageState:PivotPageState
        ) extends AbstractPivotPage(pivotPageState) {
  def text = "Select Trades"
  override def icon = StarlingIcons.im("/icons/16x16_trades.png")
  override def layoutType = Some("TradeSelection")
  def selfPage(pps:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pps)

  private def tradeSelection = {
    val deskToUse = tpp.deskAndTimestamp.map(_._1)
    val intradaySubgroupToUse = tpp.intradaySubgroupAndTimestamp.map(_._1)
    TradeSelection(deskToUse, TradePredicate(List(), List()), intradaySubgroupToUse)
  }

  private def tradeSelectionWithTimestamp = {
    TradeSelectionWithTimestamp(tpp.deskAndTimestamp, TradePredicate(List(), List()), tpp.intradaySubgroupAndTimestamp)
  }

  def dataRequest(pageBuildingContext:PageBuildingContext) = {
    val expiryDay = tpp.expiry.exp
    pageBuildingContext.cachingStarlingServer.tradePivot(tradeSelectionWithTimestamp, expiryDay, pivotPageState.pivotFieldParams)
  }
  
  override def subClassesPageData(pageBuildingContext:PageBuildingContext) = {
    val desks = pageBuildingContext.starlingServer.desks
    val admin = pageBuildingContext.starlingServer.permissionToDoAdminLikeThings
    Some(TradeSelectionPageData(tpp.deskAndTimestamp.map(_._1), desks, tpp.intradaySubgroupAndTimestamp.map(_._1), admin, pivotPageState))
  }

  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, ctrlDown:Boolean) = {
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
        pageContext.createAndGoTo(
          (starlingServer:StarlingServer) => {
            SingleTradePage(trID, tradeSelection.desk, tpp.expiry, tradeSelection.intradaySubgroup)
          }, newTab = ctrlDown)
      }
      case None => None
    }
  }

  override def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension) = {
    val tradeSelectionPageData = data match {
      case v:PivotTablePageData => v.subClassesPageData match {
        case x:Option[_] => x.get.asInstanceOf[TradeSelectionPageData]
      }
    }
    new TradeSelectionComponent(
      text, context, tradeSelectionPageData, tpp.deskAndTimestamp.map(_._2), tpp.intradaySubgroupAndTimestamp.map(_._2),
      tpp.expiry,
      PivotComponent(text, context, toolbarButtons(context, data), None, finalDrillDownPage, selfPage, data,
        pivotPageState, PivotEdits.Null, save, bookmark, browserSize))
  }

  override def refreshFunctions = {
    val functions = new ListBuffer[PartialFunction[Event, Page]]
    tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp match {
      case Some((groups, _)) => functions += {
        case IntradayUpdated(group, _, timestamp) if groups.subgroups.contains(group) => {
          val newTPP = tpp.copy(intradaySubgroupAndTimestamp = tradeSelectionWithTimestamp.copyWithNewIntradaySubgroupTimestamp(timestamp).intradaySubgroupAndTimestamp)
          this.copy(tpp = newTPP)
        }
      }
      case _ =>
    }

    functions.toList
  }

  override def bookmark(server:StarlingServer):Bookmark = {
    val today = Day.today()
    val isLatestLiveOn = tpp.expiry.exp == today
    val latestTimestamp = tpp.deskAndTimestamp.map{case (desk, t) => (t, server.latestTradeTimestamp(desk))}
    val isLatestBookClose = latestTimestamp match {
      case Some((t1,t2)) => t1 == t2
      case _ => true
    }
    val desk = tpp.deskAndTimestamp.map(_._1)
    val intraday = tpp.intradaySubgroupAndTimestamp.map(_._1)
    if (isLatestLiveOn && isLatestBookClose) {
      TradeSelectionBookmark(desk, intraday, pivotPageState, false)
    } else {
      val isLiveOnStartOfYear = latestTimestamp match {
        case Some((ts, _)) => ts.closeDay.startOfFinancialYear == tpp.expiry.exp
        case None => today.startOfFinancialYear == tpp.expiry.exp
      }
      if (isLiveOnStartOfYear && isLatestBookClose) {
        TradeSelectionBookmark(desk, intraday, pivotPageState, true)
      } else {
        PageBookmark(this)
      }
    }
  }
}

case class TradeSelectionBookmark(desk:Option[Desk], intradaySubgroups:Option[IntradayGroups],
                                  pivotPageState:PivotPageState, useStartOfYear:Boolean) extends Bookmark {
  def daySensitive = false
  def createPage(day:Option[Day], server:StarlingServer, context:PageContext) = {
    val latestBookClose = desk.map{desk => (desk, context.localCache.latestTimestamp(desk).get)}
    val latestIntraday = intradaySubgroups.map(intra => (intra, context.localCache.latestTimestamp(intra)))
    val today = Day.today()
    val tradesLiveOn = if (useStartOfYear) {
      latestBookClose match {
        case Some((_,timestamp)) => timestamp.closeDay.startOfFinancialYear
        case _ => today.startOfFinancialYear
      }
    } else {
      today
    }

    val tpp = TradePageParameters(latestBookClose, latestIntraday, TradeExpiryDay(tradesLiveOn))
    TradeSelectionPage(tpp, pivotPageState)
  }
}

case class TradeSelectionPageData(desk:Option[Desk], desks:List[Desk],
                               intradaySubgroup:Option[IntradayGroups],
                               admin: Boolean, pivotPageState:PivotPageState) extends PageData

object TimestampChooser {
  val defaultUnitialisedValue: TradeTimestamp = TradeTimestamp(new Timestamp, Day(1980, 1, 1), -1, None)
}

class TimestampChooser(initialSelection:Option[TradeTimestamp], desk: Option[Desk], pageContext: PageContext)
        extends ComboBox[TradeTimestamp](List(TimestampChooser.defaultUnitialisedValue)) {
  val initialValues: Array[Object] = (pageContext.localCache.deskCloses(desk) match {
          case Nil =>  List(TimestampChooser.defaultUnitialisedValue)
          case l => l
        }).toArray

  renderer = new ListView.Renderer[TradeTimestamp] {
    val label = new Label() {
      horizontalAlignment = swing.Alignment.Left
    }
    def componentFor(list:ListView[_], isSelected:Boolean, focused:Boolean, value:TradeTimestamp, index:Int) = {
      value.error match {
        case None => {
          label.text = value.asString
          label.foreground = Color.BLACK
        }
        case Some(_) => {
          label.text = "<html><s>" + value.asString + "</s></html>"
          label.foreground = Color.RED
        }
      }
      label
    }
  }

  val model = new DefaultComboBoxModel(initialValues)
  peer.setModel(model)

  def validSelection = desk.isDefined && initialValues.head != TimestampChooser.defaultUnitialisedValue

  enabled = validSelection

  initialSelection match {
    case Some(is) => selection.item = is
    case None =>
  }

  def selectedTimestamp: TradeTimestamp = selection.item

  def maxTimestamp: TradeTimestamp = model.getElementAt(0).asInstanceOf[TradeTimestamp]

  def selectedTimestamp_=(tradeTimestamp:TradeTimestamp) {
    selection.item = tradeTimestamp
  }

  listenTo(selection, pageContext.remotePublisher)
  reactions += {
    case SelectionChanged(c) => {
      selectedTimestamp.error match {
        case None => foreground = Color.BLACK
        case Some(_) => foreground = Color.RED
      }
    }
    case dc@DeskClosed(desk, timestamp) => {
      if (Some(desk) == this.desk && (0 until model.getSize).forall(i => model.getElementAt(i) != timestamp)) {
        model.insertElementAt(timestamp, 0)
        this.suppressing(this) {publish(dc)}
      }
    }
    case DeskCloseFailed(desk, timestamp, _) => {
      if (Some(desk) == this.desk && (0 until model.getSize).forall(i => model.getElementAt(i) != timestamp)) {
        model.insertElementAt(timestamp, 0)
      }
    }
  }
}

class TradeSelectionComponent(
        text:String,
        pageContext:PageContext,
        pageData:PageData,
        deskTimestamp: Option[TradeTimestamp],
        intradayTimestamp: Option[Timestamp],
        expiry:TradeExpiryDay,
        pivotComponent:PivotComponent) extends MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p][p][p][p]push[p]") with PageComponent {
  private val data = pageData match {case selectTradesData:TradeSelectionPageData => {selectTradesData}}

  private val deskCheckBox = new CheckBox {
    text = "Main Desk:"
    def enable_? = !data.desks.isEmpty
    enabled = enable_?
    selected = data.desk.isDefined && enabled
  }

  private val deskCombo = if (data.desks.isEmpty) new ComboBox(List(Desk(""))) else new ComboBox(data.desks) {
    renderer = ListView.Renderer(_.name)
    data.desk match {
      case Some(d) => selection.item = d
      case None => pageContext.getSettingOption(StandardUserSettingKeys.DeskDefault) match {
        case Some(defaultDesk) => selection.item = defaultDesk
        case None =>
      }
    }
  }

  private val timestampsCombo = new TimestampChooser(deskTimestamp, data.desk, pageContext)

  private val tradeExpiryDayChooser = new DayChooser(expiry.exp, false) {
    def item:TradeExpiryDay = TradeExpiryDay(day)
    def item_=(ted:TradeExpiryDay) = (day = ted.exp)
  }

  private val intradayTradesCheckBox = new CheckBox {
    text = "Intraday Trades:"
    def enable_? = !pageContext.localCache.intradaySubgroups.isEmpty
    enabled = enable_?
    selected = data.intradaySubgroup.isDefined && enabled

    listenTo(pageContext.remotePublisher)
    reactions += {
      case IntradayUpdated(_, _, _) => enabled = enable_?
    }
  }

  private def generateIntradayTradesSelection:(TreePivotFilter,Selection) = {
    def treePivotFilter = new IntradayGroupBuilder(pageContext).root
    data.intradaySubgroup match {
      case Some(i) => {
        (treePivotFilter, SomeSelection(i.subgroups.toSet))
      }
      case None => {
        // see if we have a saved selection in the user settings
        val selection = pageContext.getSettingOption(StandardUserSettingKeys.IntradayGroupsDefault) match {
          case Some(sel) => sel
          case None => Nil
        }
        (treePivotFilter, SomeSelection(selection.toSet))
      }
    }
  }

  private val intradayTradesCombo = new TreePanelComboBox(generateIntradayTradesSelection) {
    def selectedSubgroups = {
      val vAndS = valuesAndSelection
      vAndS._2 match {
        case SomeSelection(s:Set[_]) => {
          val set = s.asInstanceOf[Set[String]]
          set.toList
        }
        case AllSelection => List(vAndS._1.root.value.toString)
      }
    }
  }

  private val enterIDLabel = new Label("Enter " + deskCombo.selection.item.name + " ID:") {
    enabled = deskCheckBox.selected
  }
  private val textIDField = new TextField(10) {
    enabled = deskCheckBox.selected
  }
  private val viewButton = new NewPageButton {
    text = "View"
    tooltip = "View the trade corresponding to the trade id entered"
    enabled = deskCheckBox.selected
    reactions += {case ButtonClicked(b) => viewTrade}
  }
  private val errorLabel = new Label(" ") {
    foreground = Color.RED
  }

  private val refreshTradesButton = new Button {
    enabled = {
      deskCheckBox.selected && {
        deskCombo.selection.item == Desk.GasolineSpec ||
        deskCombo.selection.item == Desk.LondonDerivatives ||
        deskCombo.selection.item == Desk.Titan
      }
    }
    icon = StarlingIcons.icon("/icons/14x14_download_data.png")
    tooltip = "Force book close"
    reactions += {case ButtonClicked(b) => refreshTrades}
  }

  private def refreshTrades {
    val request = BookCloseRequest(deskCombo.selection.item)
    val message = "Are you sure you want to force a book close?"
    val description =
      """This book close will only effect production Starling and not other systems. A book close will take serveral minutes. The import will be complete when the book close pop-up appears."""
    pageContext.submitYesNo(
      message,
      description,
      request,
      (p:Unit) => {false},
      (p:Unit) => {}
    )
  }

  private def viewTrade {
    errorLabel.text = " "
    val desk = deskCombo.selection.item
    val textID = textIDField.text
    pageContext.createAndGoTo(
      (starlingServer:StarlingServer) => {
        val tradeID = starlingServer.tradeIDFor(desk, textID)
        SingleTradePage(tradeID, Some(desk), expiry, None)
      }, { case e:UnrecognisedTradeIDException => {
        errorLabel.text = e.getMessage
      }}
    )
  }

  private def generateNewPageFromState(resetBookClose:Boolean=false) {
    val intraSelected = intradayTradesCheckBox.selected
    val intradayItem = if (intraSelected) Some(IntradayGroups(intradayTradesCombo.selectedSubgroups)) else None
    val deskSelection = deskCombo.selection.item
    val dSelected = deskCheckBox.selected
    val desk = (if (dSelected) Some(deskSelection) else None)
    val expiry:TradeExpiryDay = tradeExpiryDayChooser.item
    pageContext.putSetting(StandardUserSettingKeys.InitialTradeSelection, (desk, intradayItem))
    intradayItem match {
      case Some(groups) => {
        pageContext.putSetting(StandardUserSettingKeys.IntradayGroupsDefault, groups.subgroups)
      }
      case None =>
    }
    desk match {
      case Some(d) => pageContext.putSetting(StandardUserSettingKeys.DeskDefault, d)
      case None =>
    }
    val deskWithTimestamp = desk.map (d => (d, {
      if ((timestampsCombo.selection.item == TimestampChooser.defaultUnitialisedValue) || resetBookClose) {
        pageContext.localCache.deskCloses(desk).headOption.getOrElse(TimestampChooser.defaultUnitialisedValue)
      } else {
        timestampsCombo.selection.item
      }
    }))
    pageContext.goTo(TradeSelectionPage(TradePageParameters(deskWithTimestamp,
      intradayItem.map(g => (g, pageContext.localCache.latestTimestamp(g))), expiry), data.pivotPageState))
  }

  private val reportButton = new NewPageButton {
    text = "Old Report"
    tooltip = "Configure a report to be run on the selected trades"
    reactions += {
      case ButtonClicked(b) => {
        val deskWithTimestamp = deskCheckBox.ifSelected((deskCombo.selection.item, timestampsCombo.selectedTimestamp))
        val intradaySubgroup = intradayTradesCheckBox.ifSelected(IntradayGroups(intradayTradesCombo.selectedSubgroups))
        val selection = getSelection
        val tradePredicate = TradePredicate(selection._1, selection._2)
        val pData = TradeAndReferenceDataInfo(tradePredicate, deskWithTimestamp, intradaySubgroup, expiry.exp)
        pageContext.goTo(ReportConfigurationPage(pData))
      }
    }
  }

  private def getSelection = pivotComponent.getSelection

  private val newReportButton = new NewPageButton {
    text = "Configure Report"
    tooltip = "Configure a report to be run on the selected trades"
    mnemonic = swing.event.Key.C
    reactions += {
      case ButtonClicked(b) => {
        val desk = deskCheckBox.ifSelected(deskCombo.selection.item)
        val deskWithTimestamp = deskCheckBox.ifSelected((deskCombo.selection.item, timestampsCombo.selectedTimestamp))
        val intradaySubgroupWithTimestamp =
          intradayTradesCheckBox.ifSelected((IntradayGroups(intradayTradesCombo.selectedSubgroups), intradayTimestamp.get))

        val selection = getSelection
        val tradeSelection = TradeSelectionWithTimestamp(deskWithTimestamp, TradePredicate(selection._1, selection._2), intradaySubgroupWithTimestamp)

        val prl = pageContext.localCache.reportOptionsAvailable.options.filter(_.slidable)

        val (initialFieldsState,otherLayoutInfo) = pageContext.getSetting(
          StandardUserSettingKeys.DefaultReportFields,
          (PivotFieldsState(
            rowFields=List(Field("Risk Market"), Field("Risk Period")),
            dataFields=List(Field("Position"))), OtherLayoutInfo())
        )

        val curveIdentifier = {
          val marketDataIdentifier = {
            val defaultSelection = MarketDataSelection(pageContext.localCache.pricingGroups(desk).headOption)
            val selection = pageContext.getSetting(
              StandardUserSettingKeys.InitialMarketDataSelection,
              defaultSelection
            )
            val version = pageContext.localCache.latestMarketDataVersionIfValid(selection)
               .getOrElse(pageContext.localCache.latestMarketDataVersion(defaultSelection))

            MarketDataIdentifier(defaultSelection, version)
          }

          CurveIdentifierLabel.defaultLabelFromSingleDay(marketDataIdentifier, pageContext.localCache.ukBusinessCalendar)
        }

        val rp = ReportParameters(
          tradeSelection,
          curveIdentifier,
          ReportOptions(prl,None,None),
          expiry.exp,
          None,
          runReports = false)
        pageContext.goTo(MainPivotReportPage(true,rp,PivotPageState(false, PivotFieldParams(true, Some(initialFieldsState)), otherLayoutInfo)))
      }
    }
  }


  private val reconciliationReportButton = new NewPageButton {
    text = "Reconciliation"
    tooltip = "Run a reconciliation between the current view and a new book close"
    def isEnabled: Boolean = {
      intradayTradesCheckBox.selected && intradayTradesCombo.selectedSubgroups.nonEmpty && timestampsCombo.selectedTimestamp.timestamp < timestampsCombo.maxTimestamp.timestamp
    }
    enabled = isEnabled
    listenTo(timestampsCombo)
    reactions += {
      case ButtonClicked(b) => {
        assert(deskCheckBox.selected, "Need a desk selected")
        assert(intradayTradesCheckBox.selected, "Need intraday trades selected")
        val tradeSystem = deskCombo.selection.item
        val intradaySubgroup = IntradayGroups(intradayTradesCombo.selectedSubgroups)
        val selection = getSelection
        val tradeSelection = TradeSelection(Some(tradeSystem), TradePredicate(selection._1, selection._2), Some(intradaySubgroup))
        val from = timestampsCombo.selectedTimestamp
        val to = timestampsCombo.maxTimestamp
        val latestIntradayTimestamp = pageContext.localCache.latestTimestamp(intradaySubgroup)

        pageContext.createAndGoTo(server => new TradeReconciliationReportPage(tradeSelection, from, to,
          latestIntradayTimestamp, data.pivotPageState))
      }
      case DeskClosed(desk, timestamp) => {
        enabled = isEnabled
      }
    }
  }

  private val tradeChangesReportButton = new NewPageButton {
    text = "Trade Changes"
    tooltip = "Shows the trade changes between the current view and a new book close"
    def isEnabled: Boolean = {
      !intradayTradesCheckBox.selected && timestampsCombo.selectedTimestamp.timestamp < timestampsCombo.maxTimestamp.timestamp
    }
    enabled = isEnabled
    listenTo(timestampsCombo)
    reactions += {
      case ButtonClicked(b) => {
        assert(deskCheckBox.selected, "Need a desk selected")
        assert(!intradayTradesCheckBox.selected, "Can't have intraday trades selected")

        val from = timestampsCombo.selectedTimestamp
        val to = timestampsCombo.maxTimestamp
        val tradeSystem = deskCombo.selection.item
        val selection = getSelection
        val tradeSelection = TradeSelection(Some(tradeSystem), TradePredicate(selection._1, selection._2), None)

        pageContext.createAndGoTo(server => TradeChangesReportPage(tradeSelection,
                from, to,
                PivotPageState(false, PivotFieldParams(true, None)), expiry.exp))
      }
      case DeskClosed(desk, timestamp) => {
        enabled = isEnabled
      }
    }
  }


  override def restoreToCorrectViewForBack = {
    this.suppressing(deskCombo.selection, deskCheckBox, intradayTradesCombo.treePanel, intradayTradesCheckBox,
      tradeExpiryDayChooser, timestampsCombo.selection) {

      pivotComponent.restoreToCorrectViewForBack
      data.desk match {
        case Some(d) => deskCombo.selection.item = d
        case None =>
      }
      data.intradaySubgroup.map(v => intradayTradesCombo.valuesAndSelection = generateIntradayTradesSelection)
      deskCheckBox.selected = data.desk.isDefined && deskCheckBox.enable_?
      intradayTradesCheckBox.selected = data.intradaySubgroup.isDefined && intradayTradesCheckBox.enable_?
      updateComponentState
    }
  }

  override def resetDynamicState {pivotComponent.resetDynamicState}

  override def getState = pivotComponent.getState
  override def setState(state:Option[ComponentState]) {pivotComponent.setState(state)}
  override def getTypeState = pivotComponent.getTypeState
  override def setTypeState(typeState:Option[ComponentTypeState]) {pivotComponent.setTypeState(typeState)}
  override def getTypeFocusInfo = pivotComponent.getTypeFocusInfo
  override def setTypeFocusInfo(focusInfo:Option[TypeFocusInfo]) {pivotComponent.setTypeFocusInfo(focusInfo)}

  private def bookCloseValid = deskTimestamp match {
    case Some(t) if t.error != None => false
    case _ => true
  }

  private def updateComponentState() {
    deskCombo.enabled = deskCheckBox.selected
    intradayTradesCombo.enabled = intradayTradesCheckBox.selected
    val buttonsEnabled = bookCloseValid && (deskCheckBox.selected || (intradayTradesCheckBox.selected && {
      intradayTradesCombo.valuesAndSelection._2 match {
        case AllSelection => true
        case SomeSelection(values) => values.nonEmpty
      }
    }))
    deskTimestamp.map(timestampsCombo.selection.item = _)
    tradeExpiryDayChooser.item = expiry
    reportButton.enabled = buttonsEnabled
    newReportButton.enabled = buttonsEnabled
    reconciliationReportButton.enabled = reconciliationReportButton.isEnabled && bookCloseValid
    tradeChangesReportButton.enabled = tradeChangesReportButton.isEnabled && bookCloseValid
  }

  updateComponentState()

  reactions += {
    case SelectionChanged(`deskCombo`) => generateNewPageFromState(true)
    case SelectionChanged(`timestampsCombo`) => generateNewPageFromState()
    case DayChangedEvent(`tradeExpiryDayChooser`, day) => generateNewPageFromState()
    case ButtonClicked(`deskCheckBox`) => generateNewPageFromState()
    case ButtonClicked(`intradayTradesCheckBox`) => generateNewPageFromState()
    case KeyPressed(`textIDField`, scala.swing.event.Key.Enter, _, _) => viewTrade
    case FilterSelectionChanged(`intradayTradesCombo`, _) => generateNewPageFromState()
  }
  listenTo(deskCheckBox, deskCombo.selection, timestampsCombo.selection, tradeExpiryDayChooser, intradayTradesCheckBox,
    intradayTradesCombo, textIDField.keys)

  val tradeInfoPanel = new MigPanel("insets 0") {
    add(new Label("Book close:"))
    add(timestampsCombo, "sg, wrap")
    add(new Label("Trades live on:"))
    add(tradeExpiryDayChooser, "sg")
  }

  add(LabelWithSeparator("Select Trade System"), "spanx, growx, wrap")
  add(deskCheckBox, "skip 1")
  add(deskCombo, "grow")
  add(refreshTradesButton)
  add(new Label(""))
  add(tradeInfoPanel, "spany 2, wrap, gapright " + RightPanelSpace)
  add(intradayTradesCheckBox, "skip 1")
  add(intradayTradesCombo, "grow, wrap unrel")
  add(LabelWithSeparator("View Individual Trade"), "spanx, growx, wrap")
  add(enterIDLabel, "skip1, split, spanx")
  add(textIDField)
  add(viewButton)
  add(errorLabel, "wrap unrel")
  add(LabelWithSeparator("Select Trades for Report"), "spanx, growx, wrap")
  add(pivotComponent, "split, spanx, grow, push, wrap unrel")
  add(reconciliationReportButton, "split, spanx, al right")
  add(tradeChangesReportButton, "al right")

  if (pageContext.localCache.currentUser.groups.contains("Starling Developers")) {
    add(newReportButton, "al right")
    add(reportButton, "al right, gapright " + RightPanelSpace)
  } else {
    add(newReportButton, "al right, gapright " + RightPanelSpace)
  }

  override def getOldPageData = pivotComponent.getOldPageData
  override def getRefreshState = pivotComponent.getRefreshState
  override def setOldPageDataOnRefresh(pageData:Option[OldPageData],
                                       refreshState:Option[ComponentRefreshState],
                                       componentState:Option[ComponentState]) =
    pivotComponent.setOldPageDataOnRefresh(pageData, refreshState, componentState)
}

case class SnapshotSubmitRequest(marketDataSelection:MarketDataSelection, observationDay:Day)
  extends SubmitRequest[Option[SnapshotIDLabel]] {

  def submit(server: StarlingServer) = server.snapshot(marketDataSelection, observationDay)
}

case class BookCloseRequest(desk:Desk) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) = {
    if (desk == Desk.Titan) {
      server.importTitanTrades()
    } else {
      server.bookClose(desk)
    }
  }
}
