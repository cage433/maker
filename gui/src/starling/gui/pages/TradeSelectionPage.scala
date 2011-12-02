package starling.gui.pages

import starling.gui._
import api._
import starling.rmi.StarlingServer
import starling.browser.common.GuiUtils._
import swing._
import event.{Event, KeyPressed, ButtonClicked, SelectionChanged}
import java.awt.{Dimension, Color}
import starling.tradestore.TradePredicate
import starling.pivot._
import collection.Seq
import controller.TreePivotFilter
import javax.swing.DefaultComboBoxModel
import starling.gui.custom._
import starling.daterange.{Timestamp, Day}
import starling.gui.StarlingLocalCache._
import starling.browser._
import common.RoundedBorder
import common.{ButtonClickedEx, NewPageButton, MigPanel}
import starling.gui.utils.RichReactor._
import starling.browser.common.RichCheckBox._
import starling.trade.facility.TradeFacility

/**
 * Page that allows you to select trades.
 */
case class TradeSelectionPage(
        tpp:TradePageParameters,
        pivotPageState:PivotPageState
        ) extends AbstractStarlingPivotPage(pivotPageState) {
  def text = "Select Trades"
  override def icon = StarlingIcons.im("/icons/16x16_trades.png")
  def selfPage(pps:PivotPageState, edits:PivotEdits) = copy(pivotPageState = pps)

  private def tradeSelection = {
    val deskToUse = tpp.deskAndTimestamp.map(_._1)
    val intradaySubgroupToUse = tpp.intradaySubgroupAndTimestamp.map(_._1)
    TradeSelection(deskToUse, TradePredicate(List(), List()), intradaySubgroupToUse)
  }

  private def tradeSelectionWithTimestamp = {
    TradeSelectionWithTimestamp(tpp.deskAndTimestamp, TradePredicate(List(), List()), tpp.intradaySubgroupAndTimestamp)
  }

  def dataRequest(pageBuildingContext:StarlingServerContext) = {
    val expiryDay = tpp.expiry.exp
    pageBuildingContext.tradeService.tradePivot(tradeSelectionWithTimestamp, expiryDay, pivotPageState.pivotFieldParams)
  }
  
  override def subClassesPageData(pageBuildingContext:StarlingServerContext) = {
    Some(TradeSelectionPageData(tpp.deskAndTimestamp.map(_._1), tpp.intradaySubgroupAndTimestamp.map(_._1), pivotPageState))
  }

  override def finalDrillDownPage(fields:Seq[(Field, Selection)], pageContext:PageContext, modifiers:Modifiers, reportSpecificChoices : ReportSpecificChoices) = {
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
        pageContext.goTo(
          SingleTradePage(trID, tradeSelection.desk, tpp.expiry, tradeSelection.intradaySubgroup),
          modifiers = modifiers
        )
      }
      case None => None
    }
  }

  override def configPanel(context:PageContext, pageData:PageData, tableSelection:() => TableSelection) = {
    val desks = context.localCache.desks
    val data = pageData match {case v:PivotTablePageData => v.subClassesPageData match {case Some(d:TradeSelectionPageData) => d}}
    val deskTimestamp = tpp.deskAndTimestamp.map(_._2)
    val expiry = tpp.expiry
    val intradayTimestamp = tpp.intradaySubgroupAndTimestamp.map(_._2)

    def bookCloseValid = deskTimestamp match {
      case Some(t) if t.error != None => false
      case _ => true
    }

    val tradeSystemConfig = new MigPanel with ConfigPanel {
      def displayName = "Trade System"

      val deskCheckBox = new CheckBox {
        text = "Main Desk:"
        def enable_? = !desks.isEmpty
        enabled = enable_?
        selected = data.desk.isDefined && enabled
      }

      val deskCombo = if (desks.isEmpty) new ComboBox(List(Desk("", Nil))) else new ComboBox(desks) {
        renderer = ListView.Renderer(_.name)
        data.desk match {
          case Some(d) => selection.item = d
          case None => context.getSettingOption(StandardUserSettingKeys.DeskDefault) match {
            case Some(defaultDesk) => selection.item = defaultDesk
            case None =>
          }
        }
      }

      val timestampsCombo = new TimestampChooser(deskTimestamp, data.desk, context) {
        minimumSize = new Dimension(50, 0)
      }

      val intradayTradesCheckBox = new CheckBox {
        text = "Intraday Trades:"
        def enable_? = !context.localCache.intradaySubgroups.isEmpty
        enabled = enable_?
        selected = data.intradaySubgroup.isDefined && enabled

        listenTo(context.remotePublisher)
        reactions += {
          case IntradayUpdated(_, _, _) => enabled = enable_?
        }
      }

      val tradeExpiryDayChooser = new DayChooser(expiry.exp, false) {
        enabled = (deskCheckBox.selected || intradayTradesCheckBox.selected)
        def item:TradeExpiryDay = TradeExpiryDay(day)
        def item_=(ted:TradeExpiryDay) = (day = ted.exp)
      }

      private def generateIntradayTradesSelection:(TreePivotFilter,Selection) = {
        def treePivotFilter = new IntradayGroupBuilder(context).root
        data.intradaySubgroup match {
          case Some(i) => {
            (treePivotFilter, SomeSelection(i.subgroups.toSet))
          }
          case None => {
            // see if we have a saved selection in the user settings
            val selection = context.getSettingOption(StandardUserSettingKeys.IntradayGroupsDefault) match {
              case Some(sel) => sel
              case None => Nil
            }
            (treePivotFilter, SomeSelection(selection.toSet))
          }
        }
      }

      def valueToLabel(v:Any):String = v.asInstanceOf[String].split("/").last
      val intradayTradesCombo = new TreePanelComboBox(generateIntradayTradesSelection, valueToLabel) {
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
        peer.setDisplayedMnemonic('e')
      }
      private val textIDField = new TextField(10) {
        enabled = deskCheckBox.selected
      }
      enterIDLabel.peer.setLabelFor(textIDField.peer)
      private val viewButton = new NewPageButton {
        text = "View"
        tooltip = "View the trade corresponding to the trade id entered"
        enabled = deskCheckBox.selected
        reactions += {case ButtonClickedEx(b, e) => viewTrade(Modifiers.modifiers(e.getModifiers))}
      }
      private val errorLabel = new Label(" ") {
        foreground = Color.RED
        visible = false
      }

      private val refreshTradesButton = new Button {
        enabled = {
          deskCheckBox.selected
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
        context.submitYesNo(
          message,
          description,
          request,
          (p:Unit) => {}
        )
      }

      private def viewTrade(mods:Modifiers) {
        errorLabel.visible = false
        errorLabel.text = " "
        val desk = deskCombo.selection.item
        val textID = textIDField.text
        context.createAndGoTo(
        (serverContext:ServerContext) => {
          val tradeID = serverContext.lookup(classOf[TradeFacility]).tradeIDFor(desk, textID)
          SingleTradePage(tradeID, Some(desk), expiry, None)
        }, { case e:UnrecognisedTradeIDException => {
          errorLabel.text = e.getMessage
          errorLabel.visible = true
        }}, modifiers = mods
        )
      }

      private def generateNewPageFromState(resetBookClose:Boolean=false) {
        val intraSelected = intradayTradesCheckBox.selected
        val intradayItem = if (intraSelected) Some(IntradayGroups(intradayTradesCombo.selectedSubgroups)) else None
        val deskSelection = deskCombo.selection.item
        val dSelected = deskCheckBox.selected
        val desk = (if (dSelected) Some(deskSelection) else None)
        val expiry:TradeExpiryDay = tradeExpiryDayChooser.item
        context.putSetting(StandardUserSettingKeys.InitialTradeSelection, (desk, intradayItem))
        intradayItem match {
          case Some(groups) => {
            context.putSetting(StandardUserSettingKeys.IntradayGroupsDefault, groups.subgroups)
          }
          case None =>
        }
        desk match {
          case Some(d) => context.putSetting(StandardUserSettingKeys.DeskDefault, d)
          case None =>
        }
        val deskWithTimestamp = desk.map (d => (d, {
          if ((timestampsCombo.selection.item == TimestampChooser.defaultUnitialisedValue) || resetBookClose) {
            context.localCache.deskCloses(desk).headOption.getOrElse(TimestampChooser.defaultUnitialisedValue)
          } else {
            timestampsCombo.selection.item
          }
        }))
        context.goTo(TradeSelectionPage(TradePageParameters(deskWithTimestamp,
          intradayItem.map(g => (g, context.localCache.latestTimestamp(g))), expiry), data.pivotPageState))
      }

      val reportButton = new NewPageButton {
        text = "Old Report"
        tooltip = "Configure a report to be run on the selected trades"
        reactions += {
          case ButtonClickedEx(b, e) => {
            val deskWithTimestamp = deskCheckBox.ifSelected((deskCombo.selection.item, timestampsCombo.selectedTimestamp))
            val intradaySubgroup = intradayTradesCheckBox.ifSelected(IntradayGroups(intradayTradesCombo.selectedSubgroups))
            val selection = tableSelection().selection
            val tradePredicate = TradePredicate(selection._1, selection._2)
            val pData = TradeAndReferenceDataInfo(tradePredicate, deskWithTimestamp, intradaySubgroup, expiry.exp)
            context.goTo(ReportConfigurationPage(pData), Modifiers.modifiers(e.getModifiers))
          }
        }
      }

      val newReportButton = new NewPageButton {
        text = "Configure Report"
        tooltip = "Configure a report to be run on the selected trades"
        mnemonic = swing.event.Key.C
        reactions += {
          case ButtonClickedEx(b, e) => {
            val desk = deskCheckBox.ifSelected(deskCombo.selection.item)
            val deskWithTimestamp = deskCheckBox.ifSelected((deskCombo.selection.item, timestampsCombo.selectedTimestamp))
            val intradaySubgroupWithTimestamp =
              intradayTradesCheckBox.ifSelected((IntradayGroups(intradayTradesCombo.selectedSubgroups), intradayTimestamp.get))

            val selection = tableSelection().selection
            val tradeSelection = TradeSelectionWithTimestamp(deskWithTimestamp, TradePredicate(selection._1, selection._2), intradaySubgroupWithTimestamp)

            val prl = context.localCache.reportOptionsAvailable.options.filter(_.slidable)

            val (initialFieldsState,otherLayoutInfo) = context.getSetting(
              StandardUserSettingKeys.DefaultReportFields,
              (PivotFieldsState(
                rowFields=List(Field("Risk Market"), Field("Risk Period")),
                dataFields=List(Field("Position"))), OtherLayoutInfo.Blank)
            )

            val curveIdentifier = {
              val marketDataIdentifier = {
                val pricingGroupsForDesk = context.localCache.pricingGroups(desk)
                val defaultSelection = MarketDataSelection(pricingGroupsForDesk.headOption)


                val selection1 = context.getSetting(
                  StandardUserSettingKeys.InitialMarketDataSelection,
                  defaultSelection
                )

                val selection2 = selection1.pricingGroup match {
                  case Some(pg) if pricingGroupsForDesk.contains(pg) => selection1
                  case _ => defaultSelection
                }

                val (version, selection) = context.localCache.latestMarketDataVersionIfValid(selection2) match {
                  case Some(v) => (v, selection2)
                  case None => (context.localCache.latestMarketDataVersion(defaultSelection), defaultSelection)
                }
                MarketDataIdentifier(selection, version)
              }

              CurveIdentifierLabel.defaultLabelFromSingleDay(marketDataIdentifier, context.localCache.ukBusinessCalendar)
            }

            val rp = ReportParameters(
              tradeSelection,
              curveIdentifier,
              ReportOptions(prl,None,None),
              expiry.exp,
              None,
              runReports = false)
            context.goTo(MainPivotReportPage(true,rp,PivotPageState(false, PivotFieldParams(true, Some(initialFieldsState)), otherLayoutInfo)), Modifiers.modifiers(e.getModifiers))
          }
        }
      }


      val reconciliationReportButton = new NewPageButton {
        text = "Reconciliation"
        tooltip = "Run a reconciliation between the current view and a new book close"
        def isEnabled: Boolean = {
          intradayTradesCheckBox.selected && intradayTradesCombo.selectedSubgroups.nonEmpty && timestampsCombo.selectedTimestamp.timestamp < timestampsCombo.maxTimestamp.timestamp
        }
        enabled = isEnabled
        listenTo(timestampsCombo)
        reactions += {
          case ButtonClickedEx(b, e) => {
            assert(deskCheckBox.selected, "Need a desk selected")
            assert(intradayTradesCheckBox.selected, "Need intraday trades selected")
            val tradeSystem = deskCombo.selection.item
            val intradaySubgroup = IntradayGroups(intradayTradesCombo.selectedSubgroups)
            val selection = tableSelection().selection
            val tradeSelection = TradeSelection(Some(tradeSystem), TradePredicate(selection._1, selection._2), Some(intradaySubgroup))
            val from = timestampsCombo.selectedTimestamp
            val to = timestampsCombo.maxTimestamp
            val latestIntradayTimestamp = context.localCache.latestTimestamp(intradaySubgroup)

            context.goTo(new TradeReconciliationReportPage(tradeSelection, from, to,
              latestIntradayTimestamp, data.pivotPageState), modifiers = Modifiers.modifiers(e.getModifiers))
          }
          case DeskClosed(desk, timestamp) => {
            enabled = isEnabled
          }
        }
      }

      val tradeChangesReportButton = new NewPageButton {
        text = "Trade Changes"
        tooltip = "Shows the trade changes between the current view and a new book close"
        def isEnabled: Boolean = {
          !intradayTradesCheckBox.selected && timestampsCombo.selectedTimestamp.timestamp < timestampsCombo.maxTimestamp.timestamp
        }
        enabled = isEnabled
        listenTo(timestampsCombo)
        reactions += {
          case ButtonClickedEx(b, e) => {
            assert(deskCheckBox.selected, "Need a desk selected")
            assert(!intradayTradesCheckBox.selected, "Can't have intraday trades selected")

            val from = timestampsCombo.selectedTimestamp
            val to = timestampsCombo.maxTimestamp
            val tradeSystem = deskCombo.selection.item
            val selection = tableSelection().selection
            val tradeSelection = TradeSelection(Some(tradeSystem), TradePredicate(selection._1, selection._2), None)

            context.goTo(TradeChangesReportPage(tradeSelection,
              from, to,
              PivotPageState(false, PivotFieldParams(true, None)), expiry.exp), modifiers = Modifiers.modifiers(e.getModifiers))
          }
          case DeskClosed(desk, timestamp) => {
            enabled = isEnabled
          }
        }
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
        case KeyPressed(`textIDField`, scala.swing.event.Key.Enter, m, _) => viewTrade(Modifiers.modifiersEX(m))
        case FilterSelectionChanged(`intradayTradesCombo`, _) => generateNewPageFromState()
      }
      listenTo(deskCheckBox, deskCombo.selection, timestampsCombo.selection, tradeExpiryDayChooser, intradayTradesCheckBox,
        intradayTradesCombo, textIDField.keys)

      val tradeSystemPanel = new MigPanel {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        add(deskCheckBox)
        add(deskCombo)
        add(refreshTradesButton)
        add(new Label("Book close:"){enabled = timestampsCombo.enabled})
        add(timestampsCombo)
      }

      val liveOnPanel = new MigPanel {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        add(new Label("Trades live on:") {enabled = tradeExpiryDayChooser.enabled})
        add(tradeExpiryDayChooser)
      }

      val intraDayPanel = new MigPanel {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        add(intradayTradesCheckBox)
        add(intradayTradesCombo)
      }

      val singleTradePanel = new MigPanel("hidemode 3") {
        border = RoundedBorder(colour = PivotTableBackgroundColour)

        add(enterIDLabel)
        add(textIDField)
        add(viewButton)
        add(errorLabel)
      }

      add(tradeSystemPanel)
      add(liveOnPanel, "wrap")
      add(intraDayPanel, "spanx, split")
      add(singleTradePanel)

      override def revert() {
        this.suppressing(deskCombo.selection, deskCheckBox, intradayTradesCombo.treePanel, intradayTradesCheckBox,
          tradeExpiryDayChooser, timestampsCombo.selection) {

          data.desk match {
            case Some(d) => deskCombo.selection.item = d
            case None =>
          }
          data.intradaySubgroup.map(v => intradayTradesCombo.valuesAndSelection = generateIntradayTradesSelection)
          deskCheckBox.selected = data.desk.isDefined && deskCheckBox.enable_?
          intradayTradesCheckBox.selected = data.intradaySubgroup.isDefined && intradayTradesCheckBox.enable_?
          updateComponentState()
        }
      }
    }

    val buttonPanel = new MigPanel("insets 0, gap 1") {
      add(tradeSystemConfig.reconciliationReportButton)
      add(tradeSystemConfig.tradeChangesReportButton)
      add(tradeSystemConfig.newReportButton)
      if (context.localCache.currentUser.isDeveloper) {
        add(tradeSystemConfig.reportButton)
      }
    }

    Some(ConfigPanels(List(tradeSystemConfig), buttonPanel, Action("BLA"){}))
  }

  override def latestPage(localCache:LocalCache) = {
    val newIntra = tpp.intradaySubgroupAndTimestamp match {
      case intra@Some((groups, timestamp)) => {
        val latestTimestamp = localCache.latestTimestamp(groups)
        if (latestTimestamp != timestamp) {
          Some((groups, latestTimestamp))
        } else {
          intra
        }
      }
      case _ => tpp.intradaySubgroupAndTimestamp
    }
    val latestDeskTimestamp = tpp.deskAndTimestamp match {
      case Some((desk, TradeTimestamp(_, TradeTimestamp.magicLatestTimestampDay, _, _))) => Some(desk, localCache.latestDeskTradeTimestamp(desk))
      case other => other
    }
    copy(tpp = tpp.copy(intradaySubgroupAndTimestamp = newIntra, deskAndTimestamp = latestDeskTimestamp))
  }

  override def bookmark(serverContext:StarlingServerContext, pd:PageData):Bookmark = {
    val today = Day.today
    val isLatestLiveOn = tpp.expiry.exp == today
    val latestTimestamp = tpp.deskAndTimestamp.map{case (desk0, t) => (t, serverContext.tradeService.latestTradeTimestamp(desk0))}
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
                                  pivotPageState:PivotPageState, useStartOfYear:Boolean) extends StarlingBookmark {
  def daySensitive = false
  def createStarlingPage(day:Option[Day], serverContext:StarlingServerContext, context:PageContext) = {
    val latestBookClose = desk.map{desk => (desk, context.localCache.latestTimestamp(desk).get)}
    val latestIntraday = intradaySubgroups.map(intra => (intra, context.localCache.latestTimestamp(intra)))
    val today = Day.today
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

case class TradeSelectionPageData(desk:Option[Desk],
                               intradaySubgroup:Option[IntradayGroups],
                               pivotPageState:PivotPageState) extends PageData

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

case class SnapshotMarketDataRequest(marketDataIdentifier:MarketDataIdentifier) extends FC2SubmitRequest[Unit] {
  def submit(fc2Context:FC2Context) = fc2Context.service.snapshot(marketDataIdentifier, SnapshotType.Manual)
}

case class ImportAndSnapshotMarketDataRequest(marketDataSelection:MarketDataSelection, observationDay:Day)
  extends FC2SubmitRequest[SnapshotIDLabel] {

  def submit(fc2Context:FC2Context) = {
    val version = fc2Context.service.importData(marketDataSelection, observationDay)
    fc2Context.service.snapshot(MarketDataIdentifier(marketDataSelection, version), SnapshotType.Manual)
  }
}

case class ImportMarketDataRequest(marketDataSelection:MarketDataSelection, observationDay:Day)
  extends FC2SubmitRequest[Unit] {

  def submit(fc2Context:FC2Context) = fc2Context.service.importData(marketDataSelection, observationDay)
}

case class BookCloseRequest(desk:Desk) extends StarlingSubmitRequest[Unit] {
  def submit(serverContext:StarlingServerContext) = {
    if (desk == Desk.Titan) {
      serverContext.tradeService.importTitanTrades()
    } else {
      serverContext.tradeService.bookClose(desk)
    }
  }
}
