package starling.gui.pages

import starling.gui._
import api._
import namedquantitycomponents.TopNamedQuantityComponent
import starling.pivot.PivotFormatter
import starling.browser.common.GuiUtils._
import starling.browser._
import common.{ImageButton, ButtonClickedEx, NewPageButton, MigPanel}
import starling.daterange.{DayAndNoTime, DayAndTime, Day}
import swing.{Component, TextArea, ScrollPane, Label}
import java.awt.{Dimension, Color}
import swing.event.MouseClicked

case class ValuationParametersPage(tradeID:TradeIDLabel, reportParameters:ReportParameters, reportSpecificChoices : ReportSpecificChoices) extends StarlingServerPage {
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
      reportParameters, tradeID)
  }
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = {
    new ValuationParametersPageComponent(context, data)
  }

  override def bookmark(serverContext: StarlingServerContext, pd:PageData) = ValuationParametersBookmark(tradeID, serverContext.reportService.createUserReport(reportParameters))
}

case class ValuationParametersBookmark(tradeID:TradeIDLabel, userReportData:UserReportData) extends StarlingBookmark {
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
    ValuationParametersPage(tradeID, reportParameters, ReportSpecificChoices())
  }
}

case class ValuationParametersPageData(tradeValuation:TradeValuationAndDetails, reportParameters:ReportParameters, tradeID:TradeIDLabel) extends PageData

object ValuationParametersPageComponent {
  def reportParametersPanel(rp:ReportParameters) = {
    new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
      add(LabelWithSeparator("Market Data Parameters"), "spanx, growx, wrap")

      def l(s:String) = new Label(s) {foreground = Color.BLUE}
      def l2(s:AnyRef) = new Label(s.toString)

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

      add(l("Observation Day"), "skip 1")
      add(l2(ci.observationDayAndTime), "wrap")

      add(l("Environment Rule"), "skip 1")
      add(l2(ci.environmentRule.name), "wrap")

      def dayAndTimeToString(dat:DayAndTime) = {
        dat match {
          case dant:DayAndNoTime => dant.day.toString
          case DayAndTime(d, tod) => d.toString + ", " + tod.shortName
        }
      }

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

      val em = rp.curveIdentifier.envModifiers
      if (em.nonEmpty) {
        add(l("Environment Modifiers"), "skip 1")
        add(l2(em.head.name), "wrap")
        for (e <- em.tail) {
          add(l2(e.name), "skip 2, wrap")
        }
      }
    }
  }
}

class ValuationParametersPageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets n n n 0") with PageComponent {
  val data = pageData.asInstanceOf[ValuationParametersPageData]

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

    var collapsed = true

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

    def toggleCollapsedPanel() {
      if (collapsed) {
        infoPanelHolder.update(infoPanel)
      } else {
        collapsedInfoPanel.preferredSize = new Dimension(infoPanel.preferredSize.width, collapsedInfoPanel.preferredSize.height)
        infoPanelHolder.update(collapsedInfoPanel)
      }
      collapsed = !collapsed
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

    toggleCollapsedPanel()

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
  }
  add(mainPanel, "push, grow")
}
