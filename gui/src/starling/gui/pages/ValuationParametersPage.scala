package starling.gui.pages

import starling.gui._
import api._
import namedquantitycomponents.{ExpandCollapseState, TopNamedQuantityComponent}
import starling.pivot.PivotFormatter
import starling.browser.common.GuiUtils._
import starling.browser._
import common._
import starling.daterange.{DayAndNoTime, DayAndTime, Day}
import swing.{Component, TextArea, ScrollPane, Label}
import swing.event.MouseClicked
import starling.gui.StarlingLocalCache._
import java.awt.{Point, Dimension, Color}

case class ValuationParametersPage(tradeID:TradeIDLabel, reportParameters:ReportParameters, reportSpecificChoices:ReportSpecificChoices, showParameters:Boolean) extends StarlingServerPage {
  def text = "Valuation Parameters for " + tradeID.id
  def icon = StarlingIcons.im("/icons/16x16_valuation_parameters.png")
  def build(reader:StarlingServerContext) = {
    val timestampToUse = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp match {
      case Some((d,ts)) => ts.timestamp
      case None => {
        reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp match {
          case None => throw new Exception("This should never happen")
          case Some((g,ts)) => ts
        }
      }
    }
    ValuationParametersPageData(
      reader.reportService.tradeValuation(tradeID, reportParameters.curveIdentifier, timestampToUse, reportSpecificChoices),
      reportParameters, tradeID, reportSpecificChoices, showParameters)
  }
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = {
    new ValuationParametersPageComponent(context, data)
  }

  override def bookmark(serverContext: StarlingServerContext, pd:PageData) = ValuationParametersBookmark(tradeID, serverContext.reportService.createUserReport(reportParameters), reportSpecificChoices, showParameters)

  override def latestPage(localCache:LocalCache) = {
    val newCurveIdentifier = localCache.latestMarketDataVersionIfValid(reportParameters.curveIdentifier.marketDataIdentifier.selection) match {
      case Some(v) => {
        reportParameters.curveIdentifier.copyVersion(v)
      }
      case _ => reportParameters.curveIdentifier
    }

    val deskAndTimestamp = reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp.map {
      case (desk, TradeTimestamp(_, TradeTimestamp.magicLatestTimestampDay, _, _)) => (desk, localCache.latestDeskTradeTimestamp(desk))
      case (desk, tradeTimestamp) => (desk , tradeTimestamp)
    }
    val intradaySubgroupAndTimestamp = reportParameters.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map {
      case (groups, _) => {
        val latestTimestamp = localCache.latestTimestamp(groups)
        (groups, latestTimestamp)
      }
    }
    val newTradeSelectionWithTimestamp = reportParameters.tradeSelectionWithTimestamp.copy(deskAndTimestamp = deskAndTimestamp, intradaySubgroupAndTimestamp = intradaySubgroupAndTimestamp)

    val newReportParameters = reportParameters.copy(curveIdentifier=newCurveIdentifier, tradeSelectionWithTimestamp = newTradeSelectionWithTimestamp)

    copy(reportParameters = newReportParameters)
  }
}

case class ValuationParametersBookmark(tradeID:TradeIDLabel, userReportData:UserReportData, reportSpecificChoices:ReportSpecificChoices, showParameters:Boolean) extends StarlingBookmark {
  def daySensitive = {
    userReportData.environmentRule match {
      case EnvironmentRuleLabel.RealTime => false
      case _ => true
    }
  }

  def createStarlingPage(day: Option[Day], serverContext: StarlingServerContext, context: PageContext) = {
    val dayToUse = day match {
      case None => Day.today // Real time
      case Some(d) => d
    }
    val reportParameters = serverContext.reportService.createReportParameters(userReportData, dayToUse)
    ValuationParametersPage(tradeID, reportParameters, reportSpecificChoices, showParameters)
  }
}

case class ValuationParametersPageData(tradeValuation:TradeValuationAndDetails, reportParameters:ReportParameters,
                                       tradeID:TradeIDLabel, reportSpecificChoices:ReportSpecificChoices, showParameters:Boolean) extends PageData

object ValuationParametersPageComponent {
  def reportParametersPanel(rp:ReportParameters) = {
    new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
      add(LabelWithSeparator("Market Data Parameters"), "spanx, growx, wrap")

      def l(s:String) = new ResizingLabel(s) {foreground = Color.BLUE}
      def l2(s:AnyRef) = new ResizingLabel(s.toString)

      val ci = rp.curveIdentifier

      ci.marketDataIdentifier.selection.pricingGroup match {
        case None =>
        case Some(pg) => {
          add(l("Pricing Group"), "skip 1")
          val extra = if (ci.marketDataIdentifier.selection.excel == None) "unrel" else ""
          add(l2(pg.name), "wrap " + extra)
        }
      }
      ci.marketDataIdentifier.selection.excel match {
        case None =>
        case Some(e) => {
          add(l("Excel Market Data"), "skip 1")
          add(l2(e), "wrap unrel")
        }
      }

      def dayAndTimeToString(dat:DayAndTime) = {
        dat match {
          case dant:DayAndNoTime => dant.day.toString
          case DayAndTime(d, tod) => d.toString + ", " + tod.shortName
        }
      }

      add(l("Observation Day"), "skip 1")
      add(l2(dayAndTimeToString(ci.observationDayAndTime)), "wrap")

      add(l("Environment Rule"), "skip 1")
      add(l2(ci.environmentRule.name), "wrap")

      add(l("Forward Observation"), "skip 1")
      add(l2(dayAndTimeToString(ci.forwardValuationDayAndTime)), "wrap")

      add(l("Theta to"), "skip 1")
      add(l2(dayAndTimeToString(ci.thetaToDayAndTime)), "wrap")

      add(l("Live on"), "skip 1")
      add(l2(rp.expiryDay), "wrap")

      val bookClose = rp.tradeSelectionWithTimestamp.deskAndTimestamp match {
        case Some((d,ts)) => ts
        case None => rp.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp match {
          case None => throw new Exception("This should never happen")
          case Some((g,ts)) => ts
        }
      }

      add(l("Book close"), "skip 1")
      add(l2(bookClose), "wrap unrel")

      rp.pnlParameters match {
        case None =>
        case Some(pnlP) => {
          add(l("Day Change"), "skip 1")
          add(l2(pnlP.curveIdentifierFrom.environmentRule.name), "wrap")
          add(l("Book Close 2"), "skip 1")
          add(l2(pnlP.tradeTimestampFrom.get), "wrap unrel")
        }
      }

      // This is a massive hack until we change zero interest rates to discounted.
      def envModName(name:String) = if (EnvironmentModifierLabel.zeroInterestRates.name == name) "Not Discounted" else name

      val em = rp.curveIdentifier.envModifiers
      if (em.nonEmpty) {
        add(l("Environment Modifiers"), "skip 1")
        add(l2(envModName(em.head.name)), "wrap")
        for (e <- em.tail) {
          add(l2(envModName(e.name)), "skip 2, wrap")
        }
      }
    }
  }
}

class ValuationParametersPageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets n n n 0") with PageComponent {
  val data = pageData.asInstanceOf[ValuationParametersPageData]
  private var explanationComponentOption:Option[TopNamedQuantityComponent] = None
  private var explanationScrollOption:Option[ScrollPane] = None
  val mainPanel = new MigPanel("insets 0") {
    val versionsButton = new NewPageButton {
      text = "Trade Versions"
      reactions += {
        case ButtonClickedEx(b, e) => {
          val rp = data.reportParameters
          val page = SingleTradePage(data.tradeID, rp.tradeSelectionWithTimestamp.desk, TradeExpiryDay(rp.expiryDay),
            rp.tradeSelectionWithTimestamp.intradaySubgroupAndTimestamp.map(_._1))
          context.goTo(page, Modifiers.modifiers(e.getModifiers))
        }
      }
    }
    val tradePanels = SingleTradePageComponent.generateTradePanels(data.tradeValuation.tradeRow,
      data.tradeValuation.fieldDetailsGroups, data.tradeValuation.columns)

    private def toggleCollapsedPanel() {
      val newPage = ValuationParametersPage(data.tradeID, data.reportParameters, data.reportSpecificChoices, !data.showParameters)
      context.goTo(newPage)
    }

    val infoPanelHolder = new MigPanel("insets 0") {
      def update(comp:Component) {
        removeAll
        add(comp, "growx")
        revalidate()
        repaint()
      }

      reactions += {case MouseClicked(_,_,_,2,_) => toggleCollapsedPanel()}
      listenTo(mouse.clicks)
    }

    val infoPanel = new MigPanel("insets 0") {
      val (tradeFieldsPanel, otherPanels) = tradePanels.partition(_._1 == "Trade Fields")
      val reportsPanel = ValuationParametersPageComponent.reportParametersPanel(data.reportParameters)
      if (tradeFieldsPanel.nonEmpty) {
        val combinedPanel = new MigPanel("insets 0") {
          add(tradeFieldsPanel.head._2, "growx, wrap")
          add(reportsPanel, "growx")
        }
        add(combinedPanel, "ay top, gapright unrel")
        otherPanels.reverse.tail.reverse.map(_._2).foreach(add(_, "ay top, gapright unrel"))
        add(otherPanels.last._2, "ay top, split, gapright 0")
      } else {
        tradePanels.reverse.tail.reverse.map(_._2).foreach(add(_, "ay top, gapright unrel"))
        add(tradePanels.last._2, "ay top, split, gapright 0")
      }

      val collapseButton = new ImageButton(StarlingIcons.im("/icons/scroll_up.png"), toggleCollapsedPanel())
      add(collapseButton, "ay top")
    }

    val collapsedInfoPanel = new MigPanel("insets 0") {
      val labelWithSeparator = LabelWithSeparator("Trade, Intrument and Market Data Parameters")
      labelWithSeparator.enabled = false
      val expandButton = new ImageButton(StarlingIcons.im("/icons/scroll_down.png"), toggleCollapsedPanel())

      add(labelWithSeparator, "ay top, pushx, growx, split, gapright 0")
      add(expandButton, "ay top")

      preferredSize = new Dimension(infoPanel.preferredSize.width, preferredSize.height)
    }

    add(infoPanelHolder, "ay top, split")
    add(versionsButton, "ay top, wrap")

    val pnl = data.tradeValuation.tradeValuation.explanation
    pnl match {
      case Right(explanation) => {
        val valuationParametersExplainPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
          val extraFormatInfo = context.getSetting(StandardUserSettingKeys.ExtraFormattingInfo, PivotFormatter.DefaultExtraFormatInfo)
          val explanationComponent = new TopNamedQuantityComponent(explanation, extraFormatInfo) {
            reactions += {
              case UserSettingUpdated(StandardUserSettingKeys.ExtraFormattingInfo) => {
                val extraFormatInfo = context.getSetting(StandardUserSettingKeys.ExtraFormattingInfo)
                updateExtraInfo(extraFormatInfo)
              }
            }
            listenTo(context.remotePublisher)
          }
          val explanationScrollPane = new ScrollPane(explanationComponent) {
            verticalScrollBar.unitIncrement = 10
            horizontalScrollBar.unitIncrement = 10
          }
          explanationComponentOption = Some(explanationComponent)
          explanationScrollOption = Some(explanationScrollPane)
          add(LabelWithSeparator("Valuation Parameters"), "spanx, growx, wrap")
          add(explanationScrollPane, "skip 1, push, grow, gapright " + RightPanelSpace)
        }

        add(valuationParametersExplainPanel, "push, grow")
      }
      case Left(error) => {
        import StarlingLocalCache._
        val valuationStackTracePanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
          add(LabelWithSeparator("Stacktrace"), "spanx, growx, wrap")
          val errorScrollPane = new ScrollPane(new TextArea(error.stackTrace))
          add(errorScrollPane, "skip 1, push, grow, gapright " + RightPanelSpace)
          visible = context.localCache.currentUser.isDeveloper
        }
        val valuationParametersErrorPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
          add(LabelWithSeparator("Error Explaining Valuation"), "spanx, growx, wrap")
          val errorLabel= new Label(error.longMessage.getOrElse(error.message)) {
            foreground = Color.RED
          }
          add(errorLabel, "skip 1, push, grow, gapright " + RightPanelSpace)
          reactions += {
            case MouseClicked(_,_,_,2,_) => {
              valuationStackTracePanel.visible = !valuationStackTracePanel.visible
            }
          }
          listenTo(mouse.clicks)
        }
        add(valuationParametersErrorPanel, "push, wrap")
        add(valuationStackTracePanel, "push, grow")
      }
    }
    if (data.showParameters) {
      infoPanelHolder.update(infoPanel)
    } else {
      infoPanelHolder.update(collapsedInfoPanel)
    }
  }
  add(mainPanel, "push, grow")

  private def state = explanationComponentOption.map(c => ValuationParametersPageComponentState(c.expandCollapseState, explanationScrollOption.get.peer.getViewport.getViewPosition))
  private def state_=(s:ValuationParametersPageComponentState) {
    explanationComponentOption.map(c => {
      c.applyExpandCollapseState(s.expandCollapseState)
    })
    explanationScrollOption.map(c => {
      c.peer.getViewport.setViewPosition(s.scrollPosition)
    })
  }

  override def getState:Option[ComponentState] = state
  override def setState(s:Option[ComponentState]) {
    s match {
      case Some(s0:ValuationParametersPageComponentState) => state = s0
      case _ =>
    }
  }
  override def getTypeState:Option[ComponentTypeState] = state
  override def setTypeState(typeState:Option[ComponentTypeState]) {
    typeState match {
      case Some(s:ValuationParametersPageComponentState) => state = s
      case _ =>
    }
  }
}

case class ValuationParametersPageComponentState(expandCollapseState:ExpandCollapseState, scrollPosition:Point) extends ComponentState with ComponentTypeState
