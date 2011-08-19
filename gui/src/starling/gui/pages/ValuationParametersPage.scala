package starling.gui.pages

import starling.utils.SColumn
import starling.gui._
import api._
import namedquantitycomponents.TopNamedQuantityComponent
import java.awt.{Color, Dimension}
import starling.quantity.SimpleNamedQuantity
import swing.{ScrollPane, Label}
import starling.pivot.PivotFormatter
import starling.browser.common.MigPanel
import starling.browser.common.GuiUtils._
import starling.browser._
import starling.rmi.StarlingServer
import starling.daterange.Day

case class ValuationParametersPage(tradeID:TradeIDLabel, reportParameters:ReportParameters) extends StarlingServerPage {
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
      reader.cachingStarlingServer.tradeValuation(tradeID, reportParameters.curveIdentifier, timestampToUse),
      reportParameters)
  }
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]) = {
    new ValuationParametersPageComponent(context, data)
  }

  override def bookmark(serverContext: StarlingServerContext) = ValuationParametersBookmark(tradeID, serverContext.server.createUserReport(reportParameters))
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
      case None => Day.today() // Real time
      case Some(d) => d
    }
    val reportParameters = serverContext.server.createReportParameters(userReportData, dayToUse)
    ValuationParametersPage(tradeID, reportParameters)
  }
}

case class ValuationParametersPageData(tradeValuation:TradeValuationAndDetails, reportParameters:ReportParameters) extends PageData

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
      add(l2(ci.tradesUpToDay), "wrap")

      add(l("Environment Rule"), "skip 1")
      add(l2(ci.environmentRule.name), "wrap")

      add(l("Forward Observation"), "skip 1")
      add(l2(ci.valuationDayAndTime.day), "wrap")

      add(l("Theta to"), "skip 1")
      add(l2(ci.thetaDayAndTime), "wrap")

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
    val tradePanels = SingleTradePageComponent.generateTradePanels(data.tradeValuation.tradeRow,
      data.tradeValuation.fieldDetailsGroups, data.tradeValuation.columns)
    val infoPanel = new MigPanel("insets 0") {
      tradePanels.foreach(add(_, "ay top, gapright unrel"))
      add(ValuationParametersPageComponent.reportParametersPanel(data.reportParameters), "ay top")
    }

    val pnl = data.tradeValuation.tradeValuation.explanation
     
    val valuationParametersExplainPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
      val extraFormatInfo = context.getSetting(StandardUserSettingKeys.ExtraFormattingInfo, PivotFormatter.DefaultExtraFormatInfo)
      val explanationComponent = new TopNamedQuantityComponent(pnl, extraFormatInfo) {
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

    add(infoPanel, "pushx, wrap")
    add(valuationParametersExplainPanel, "push, grow")
  }
  add(mainPanel, "push, grow")
}
