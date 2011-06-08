package starling.gui.pages

import starling.gui._
import api._
import custom.painters.StripedCornerPainter
import scala.swing.Swing._
import java.awt.{Font, Color, Dimension, Cursor}
import java.awt.event.KeyEvent
import starling.auth.User
import org.jdesktop.swingx.painter.{CompoundPainter, GlossPainter}
import collection.immutable.TreeMap
import scala.swing._
import event._
import javax.swing.{KeyStroke, JComponent}
import starling.daterange.{ObservationTimeOfDay, ObservationPoint, Day}
import starling.pivot.view.swing._
import starling.pivot._

case class StarlingHomePage() extends Page {
  def build(reader:PageBuildingContext) = {HomePagePageData(reader.cachingStarlingServer.version, reader.cachingStarlingServer.desks.headOption, reader.starlingServer.whoAmI)}
  def createComponent(context: PageContext, data: PageData, browserSize:Dimension) = new StarlingHomePageComponent(context, browserSize, data)
  def text = "Starling"
  val icon = StarlingIcons.im("/icons/weather-clear.png")
}

case class HomePagePageData(version:Version, initialDesk:Option[Desk], user:User) extends PageData

class StarlingHomePageComponent(context:PageContext, browserSize:Dimension, pageData:PageData) extends MigPanel("insets 0") with PageComponent {
  private val data = pageData match {case d:HomePagePageData => {d}}

  private val reportsPanel = new MigPanel("") {
    border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
    background = GuiUtils.TaskPageButtonBackgroundColour

    val iconLabel = new Label {
      icon = StarlingIcons.icon("/icons/32x32_report_star.png")
    }
    val textLabel = new Label("Select a Valuation Day, Pre-Configured Report and Layout")
    val userReports = context.localCache.userReports

    val bkColour = new Color(228, 231, 246)
    val reportsListView = new NListView(userReports) {
      renderer = ListView.Renderer(_.reportName)
      background = bkColour
    }
    val userLayouts = context.localCache.userPivotLayouts
    val layoutsListView = new NListView(List(PivotLayout.BlankLayout)) {
      renderer = ListView.Renderer(_.layoutName)
      background = bkColour
    }
    val reportDayChooser = new DayChooser(enableFlags = false) {
      background = bkColour
    }

    def deleteReport {
      val userReport = reportsListView.selected
      context.submitYesNo("Are you sure you want to delete the \"" + userReport.reportName + "\" report configuration?",
          "Layouts only associated with this report will be deleted as well",
          DeleteReportRequest(userReport.reportName), (u:Unit) => {false}, (u:Unit) => {})
    }

    def deleteLayout {
      val userLayout = layoutsListView.selected
      if(userLayout != PivotLayout.BlankLayout) {
        context.submitYesNo(
            "Are you sure you want to delete the " + userLayout.layoutName + " layout?", "",
            DeletePivotLayoutRequest(userLayout.layoutName), (u:Unit) => {false}, (u:Unit) => {})
      }
    }

    def runReport {
      val userReport = reportsListView.selected
      val userLayout = layoutsListView.selected
      val baseDay = reportDayChooser.day
      val observationDaysForPricingGroup = context.localCache.populatedObservationDaysForPricingGroup
      context.createAndGoTo((server) => {
        val reportParameters = server.createReportParameters(userReport.data, baseDay)
        MainPivotReportPage(userReport.showParameters, reportParameters,
          PivotPageState(false, PivotFieldParams(true, Some(userLayout.pivotFieldState)), userLayout.otherLayoutInfo))
      })
    }

    val runReportAction = Action("Run"){runReport}
    runReportAction.toolTip = "Run the selected pre-configured report (F9)"
    runReportAction.icon = StarlingIcons.icon("/icons/16x16_report.png")

    val runButton = new Button {       
      background = GuiUtils.TaskPageBackgroundColour
      action = runReportAction
    }
    val reportScrollPane = new ScrollPane(reportsListView)
    if (userReports.size <= 3) {
      reportScrollPane.preferredSize = new Dimension(preferredSize.width, 100)
    }
    val layoutScrollPane = new ScrollPane(layoutsListView)
    if (userLayouts.size <= 3) {
      layoutScrollPane.preferredSize = new Dimension(preferredSize.width, 100)
    }

    add(iconLabel, "split 3, spanx")
    add(textLabel, "gapright unrel")
    add(runButton, "tag ok, wrap")
    add(reportDayChooser, "split, spanx, gapleft 10lp, ay top")
    add(reportScrollPane, "push, grow, sg")
    add(layoutScrollPane, "push, grow, sg, gapright 10lp")

    def componentsEnabled = false // Not used
    def componentsEnabled_=(b:Boolean) {
      runReportAction.enabled = b
      reportDayChooser.enabled = b
      reportScrollPane.enabled = b
      layoutScrollPane.enabled = b
      reportsListView.enabled = b
      layoutsListView.enabled = b
      iconLabel.enabled = b
      textLabel.enabled = b
    }

    def updateLayouts {
      val currentlySelected = layoutsListView.selectedOption
      val currentLayouts = PivotLayout.BlankLayout :: context.localCache.userPivotLayouts
      val layoutsToUse = currentLayouts.filter(pl => {
        if (reportsListView.selection.indices.nonEmpty) {
          pl.layoutType == PivotLayout.ReportLayoutType && pl.associatedReports.contains(reportsListView.selected.reportName)
        } else {
          true
        }
      })
      layoutsListView.listData = (PivotLayout.BlankLayout :: layoutsToUse.reverse).reverse
      layoutsListView.selectedOption = currentlySelected
    }

    reactions += {
      case UserReportUpdate(username,userReports) if (username == data.user.username) => {
        val currentSelectedItem = reportsListView.selectedOption
        reportsListView.listData = userReports
        if (reportsListView.listData.isEmpty) {
          componentsEnabled = false
        } else {
          componentsEnabled = true
        }
        reportsListView.selectedOption = currentSelectedItem
      }
      case PivotLayoutUpdate(username, layouts) if (username == data.user.username) => updateLayouts
      case MouseClicked(`reportsListView`,_,_,2,_) => {runReport}
      case MouseClicked(`layoutsListView`,_,_,2,_) => {runReport}
      case KeyPressed(`reportsListView`, scala.swing.event.Key.Delete, _, _) => {deleteReport}
      case KeyPressed(`layoutsListView`, scala.swing.event.Key.Delete, _, _) => {deleteLayout}
      case SelectionChanged(`reportsListView`) => updateLayouts
    }
    listenTo(context.remotePublisher, reportsListView.keys, reportsListView.mouse.clicks, layoutsListView.keys, layoutsListView.mouse.clicks, reportsListView.selection)

    if (!userReports.isEmpty) {
      reportsListView.selectIndices(0)
    }
    if (!userLayouts.isEmpty) {
      layoutsListView.selectIndices(0)
    }

    componentsEnabled = userReports.nonEmpty
  }

  override def getState = {
    Some(StarlingHomePageComponentState(reportsPanel.reportsListView.selectedOption, reportsPanel.layoutsListView.selectedOption))
  }

  override def setState(state:Option[ComponentState]) = {
    state match {
      case Some(StarlingHomePageComponentState(selectedReport, selectedLayout)) => {
        reportsPanel.reportsListView.selectedOption = selectedReport
        reportsPanel.layoutsListView.selectedOption = selectedLayout
      }
      case _ =>
    }
  }

  private val versionPanel = new MigPanel("") {
    border = LineBorder(GuiUtils.TaskPageButtonBorderColour)
    background = GuiUtils.TaskPageButtonBackgroundColour
   
    val ver = data.version
    if (ver.production) {
      add(new Label("<html><b>Production</b></html>"), "ax center, gapleft 100lp, gapright 100lp")
    } else {
      add(new Label("<html><b>" + data.version.name + " (" + data.version.hostname + ")</b></html>"), "ax center, wrap")
      add(new Label(data.version.database), "span, ax center")
    }
  }
    
  val c = new MigPanel("insets 0", "[grow,fill]", "[p]0[grow,fill]") {
    val banner = new MigXPanel("insets 0", "[p][p][p]push[p]") {
      background = GuiUtils.BannerColour
      val gp = new GlossPainter
      val sp = StripedCornerPainter(new Color(0,0,200))
      backgroundPainter = new CompoundPainter(sp,gp)
      
      val logoImage = StarlingIcons.im("/icons/small_sunny_bird2.png")
      val logo = new FixedImagePanel(logoImage)

      val nameLabel = new Label {
        text = "Starling"
        font = new Font("Lucida Sans", Font.PLAIN, 30)
      }
      val welcomeLabel = new Label {
        text = "W E L C O M E !"
        font = new Font("Dialog", Font.BOLD, 60)
        foreground = new Color(255,221,138)
      }
      val userImage = StarlingIcons.im("/icons/32x32_user_dark.png")
      val userButton = new ReferenceDataButton(data.user.name, userImage, ctrlDown => context.goTo(UserDetailsPage(data.user), ctrlDown), false)
      userButton.label.font = new Font("Serif", Font.PLAIN, 20)

      add(logo)
      add(nameLabel, "ay bottom, gapbottom 5lp")
      add(welcomeLabel, "ay center, gapleft 20lp")
      add(userButton, "ay center, gapright " + GuiUtils.StandardLeftIndent)
    }

    val actionsPanelHolder = new MigPanel("insets dialog") {
      val actionsPanel = new StripedPanel("insets 0", "[grow][p][p][p][p][grow]", "[grow][p][p][grow 150][p]") {
        val tradeDataImage = StarlingIcons.im("/icons/32x32_trades.png")
        val tradeDataButton = new ReferenceDataButton("Trades", tradeDataImage, gotoTradePage, number = Some("1."))

        val refDataImage = StarlingIcons.im("/icons/32x32_ref_data.png")
        val refDataButton = new ReferenceDataButton("Reference Data", refDataImage, ctrlDown => {context.goTo(ReferenceDataIndexPage, ctrlDown)}, number = Some("2."))

        val marketDataImage = StarlingIcons.im("/icons/32x32_market_data.png")
        val marketDataButton = new ReferenceDataButton("Market Data", marketDataImage, gotoMarketDataPage, number = Some("3."))

        val curveViewerImage = StarlingIcons.im("/icons/32x32_curve_viewer.png")
        val curveViewerButton = new ReferenceDataButton("Curve Viewer", curveViewerImage, gotoCurvePage, number = Some("4."))

        val helpLabelHolder = new MigPanel {
          background = new Color(0,0,0,0)
          opaque = false

          val helpImage = StarlingIcons.im("/icons/32x32_Help.png")
          add(new FixedImagePanel(helpImage) {
            cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
            reactions += {
              case MouseClicked(_,_,_,_,_) => {
                context.goTo(HelpPage)
              }
            }
            listenTo(mouse.clicks)
          }, "push,grow")
        }

        add(helpLabelHolder, "split, spanx, ax right, ay top, wrap")
        add(tradeDataButton, "sg, skip 1")
        add(refDataButton, "sg")
        add(marketDataButton, "sg")
        add(curveViewerButton, "sg, wrap unrel")

        add(reportsPanel, "spanx, ax center, wrap")
        add(versionPanel, "newline, split, spanx, ax center, gapbottom 5lp")
      }
      add(actionsPanel, "push,grow")
    }

    add(banner, "wrap")
    add(actionsPanelHolder)
  }

  override def pageShown = reportsPanel.runButton.requestFocusInWindow

  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_1, 0), "tradesAction")
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_2, 0), "refDataAction")
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_3, 0), "marketDataAction")
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_4, 0), "curveAction")
  peer.getActionMap.put("tradesAction", Action("tradesAction"){gotoTradePage(false)}.peer)
  peer.getActionMap.put("refDataAction", Action("refDataAction"){context.goTo(ReferenceDataIndexPage)}.peer)
  peer.getActionMap.put("marketDataAction", Action("marketDataAction"){gotoMarketDataPage(false)}.peer)
  peer.getActionMap.put("curveAction", Action("curveAction"){gotoCurvePage(false)}.peer)

  private val runAct = reportsPanel.runReportAction
  peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), runAct.title)
  peer.getActionMap.put(runAct.title, runAct.peer)

  private def gotoTradePage(ctrlDown:Boolean) {
    context.goTo( {
      val initial = {
        val defaultSelection = (data.initialDesk, None) 
        val lastSelection = context.getSetting(StandardUserSettingKeys.InitialTradeSelection, defaultSelection)
        lastSelection match {
          case (_, Some(groups)) => {
            val validGroups = context.localCache.intradaySubgroups.keySet
            if (groups.subgroups.forall(g => validGroups.exists(vg => vg.startsWith(g)))) lastSelection else defaultSelection
          }
          case _ => lastSelection
        }
      }

      val deskWithTime = initial._1.flatMap(d => context.localCache.latestTimestamp(d).map(ts => (d, ts)))
      val intradayWithTime = initial._2.map(groups => (groups, context.localCache.latestTimestamp(groups)))

      TradeSelectionPage(
        deskWithTime, intradayWithTime,
        TradeSelectionComponentState(initial._1.isDefined, initial._2.isDefined, PivotPageState(false, PivotFieldParams(true, None))),
        TradeExpiryDay(Day.today))
    }, ctrlDown)
  }

  def gotoMarketDataPage(ctrlDown:Boolean) {
    context.goTo(new MarketDataPage(
      defaultMarketDataIdentifier,
      MarketDataPageState()
    ), ctrlDown)
  }

  def gotoCurvePage(ctrlDown: Boolean) {
    val curveLabel = CurveLabel(CurveTypeLabel("Price"), defaultMarketDataIdentifier, EnvironmentSpecificationLabel(
      context.localCache.populatedDays(defaultMarketDataIdentifier.selection).lastOption.getOrElse(Day.today),
      context.localCache.environmentRulesForPricingGroup(defaultMarketDataIdentifier.selection.pricingGroup).head
    ))

    val initialState = PivotFieldsState(
      dataFields = List(Field("Price"), Field("Input")),
      columnFields = List(Field("Observation Time"), Field("Market")),
      rowFields = List(Field("Period"))
    )

    context.goTo(new CurvePage(curveLabel, PivotPageState.default(initialState)), ctrlDown)
  }

  private def defaultMarketDataIdentifier: MarketDataIdentifier = {
    val initialSelection = context.getSetting(StandardUserSettingKeys.InitialMarketDataSelection,
      MarketDataSelection(context.localCache.pricingGroups(None).headOption))
    val latestMarketDataVersion = context.localCache.latestMarketDataVersion(initialSelection)

    MarketDataIdentifier(initialSelection, latestMarketDataVersion)
  }

  add(c, "push,grow")

//  context.setDefaultButton(Some(reportsPanel.runButton))
}

case class StarlingHomePageComponentState(reportSelected:Option[UserReport], layoutSelected:Option[PivotLayout]) extends ComponentState
