package starling.gui.pages

import starling.gui.api._
import starling.gui._
import starling.pivot.view.swing.MigPanel
import starling.gui.GuiUtils._
import javax.swing.ListSelectionModel
import starling.pivot._
import swing.event.ButtonClicked
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import starling.tradestore.TradePredicate
import java.awt.{Color, Dimension}
import org.jdesktop.swingx.decorator.{ColorHighlighter, HighlightPredicate}
import java.awt.event.{MouseEvent, MouseAdapter}
import collection.mutable.ListBuffer
import starling.utils.{SColumn, STable}
import starling.daterange.{TimeOfDay, Timestamp}
import collection.immutable.TreeMap
import swing.{Component, Button, Label}
import starling.quantity.Quantity

case class SingleTradePage(tradeID:TradeIDLabel, desk:Option[Desk], tradeExpiryDay:TradeExpiryDay, intradayGroups:Option[IntradayGroups]) extends Page {
  def text = "Trade " + tradeID
  def icon = StarlingIcons.im("/icons/tablenew_16x16.png")
  def build(reader:PageBuildingContext) = TradeData(tradeID, reader.cachingStarlingServer.readTradeVersions(tradeID), desk, tradeExpiryDay, intradayGroups)
  def createComponent(context:PageContext, data:PageData, browserSize:Dimension) = new SingleTradePageComponent(context, data)
}

object SingleTradePageComponent {
  def generateTradePanels(tradeRow:List[Any], fieldDetailsGroups:List[FieldDetailsGroupLabel], columns:List[SColumn]) = {
    val columnIndexMap = Map() ++ columns.map(_.name).zipWithIndex
    val panelBuffer = new ListBuffer[MigPanel]
    for (group <- fieldDetailsGroups) {
      val groupPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
        add(LabelWithSeparator(group.groupName), "spanx, growx, wrap")
        for (fieldName <- group.childNames) {
          if (columnIndexMap.contains(fieldName)) {
            add(new Label(fieldName) {foreground = Color.BLUE}, "ay top, skip 1")
            val value = tradeRow(columnIndexMap(fieldName)).asInstanceOf[TableCell]
            val valueLabel = new Label {
              if (fieldName == "Strategy") {
                // Make a special case for strategy so that it doesn't take up too much horizontal space.
                val paths = value.text.split("/")
                val textToUse = (paths.zipWithIndex.map{case (path,index) => {
                  ("&nbsp" * (2 * index - 1)) + ("|-" * math.min(1,index)) + path
                }}).mkString("<BR>")
                text = "<html>" + textToUse + "</html>"
              } else {
                text = value.text
              }
              value.doubleValueIgnoringErrors match {
                case None =>
                case Some(v) => if (v < 0.0) foreground = Color.RED
              }
              maximumSize = new Dimension(500, Integer.MAX_VALUE)
            }
            add(valueLabel, "wrap")
          }
        }
      }
      panelBuffer += groupPanel
    }
    panelBuffer.toList
  }

  def generateCostsPanel(costs:CostsLabel) = {
    new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
      val columns = List(SColumn("Cost Type"), SColumn("Value"))
      class MyPivotQuantity(val value:Quantity) extends PivotQuantity(Map(value.uom -> value.value), Map[String,List[StackTrace]]()) {
        override def toString = PivotFormatter.formatPivotQuantity(this, PivotFormatter.DefaultExtraFormatInfo)
      }
      val data = costs.costInfo.map(c => {
        List(c.name, new MyPivotQuantity(c.value))
      })
      val costsChooser = new TableTable(STable("Costs Chooser", columns, data))
      val heightToUse = if (costsChooser.jTable.getPreferredSize.height < 150) 150 else costsChooser.jTable.getPreferredScrollableViewportSize.height
      costsChooser.jTable.setPreferredScrollableViewportSize(new Dimension(costsChooser.jTable.getPreferredSize.width, heightToUse))
      costsChooser.jTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      val negativeHighlighter = new ColorHighlighter(new HighlightPredicate {
        def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
          adapter.getValue match {
            case p:MyPivotQuantity => p.value.value < 0.0
            case _ => false
          }
        }
      })
      negativeHighlighter.setForeground(Color.RED)
      costsChooser.jTable.addHighlighter(negativeHighlighter)
      costsChooser.jTable.getSelectionModel.addListSelectionListener(new ListSelectionListener {
        def valueChanged(e:ListSelectionEvent) = {
          if (!e.getValueIsAdjusting) {
            updateInfoPanel(costsChooser.jTable.getSelectedRow)
          }
        }
      })
      val infoPanel = new MigPanel("insets 0") {
        def update(comp:Component) {
          removeAll
          add(comp, "push,grow")
          revalidate
          repaint
        }
      }
      def updateInfoPanel(index:Int) {
        val newPanel = new MigPanel {
          val info = costs.costInfo(index).info
          info.foreach{case (key,value) => {
            add(new Label(key) {foreground = Color.BLUE})
            add(new Label(value), "wrap")
          }}
        }
        infoPanel.update(newPanel)
      }

      if (costs.costInfo.nonEmpty) {
        costsChooser.jTable.getSelectionModel.setSelectionInterval(0,0)
        updateInfoPanel(0)
      }

      add(LabelWithSeparator("Costs"), "spanx, growx, wrap")
      add(costsChooser, "skip 1")
      add(infoPanel, "ay top")
    }
  }

  val DefaultReportSpecificChoices = TreeMap(
    "Lots" -> false,
    "Position" -> "Default",
    "Tenor" -> "M",
    "Futures as swaps" -> false,
    "Show Eq Futures" -> false,
    "Skew" -> true,
    "Futures as spreads" -> false,
    "ATM Vega" -> false,
    "Collapse Options" -> true)

  val GreeksLayout = new PivotFieldsState(
    rowFields=List(
      Field("Trade ID"),
      Field("Risk Market"),
      Field("Risk Period")
    ),
    columns = ColumnTrees(List(
    ColumnTree(Field("Instrument"), false,
      List("Position", "Market Price", "Initial Price", "P&L", "Quantity").map(f=>ColumnTree(Field(f), true)): _*)
    )),
    reportSpecificChoices = DefaultReportSpecificChoices
  )
}

class SingleTradePageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets 0") with PageComponent {
  val data = pageData.asInstanceOf[TradeData]
  val (stable, fieldDetailsGroups, costs) = data.tradeHistory
  val columnIndexMap = Map() ++ stable.columns.zipWithIndex.map{case (col,index) => (col.name,index)}

  val mainPanel = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
    val historyTable = new TableTable(stable)
    historyTable.jTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    val heightToUse = if (historyTable.jTable.getPreferredSize.height < 150) 150 else historyTable.jTable.getPreferredScrollableViewportSize.height
    historyTable.jTable.setPreferredScrollableViewportSize(new Dimension(historyTable.jTable.getPreferredSize.width, heightToUse))
    val rowToSelect = stable.data.size - 1
    historyTable.jTable.setRowSelectionInterval(rowToSelect, rowToSelect)
    historyTable.jTable.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e:ListSelectionEvent) = {
        if (!e.getValueIsAdjusting) {
          updateTradePanel
        }
      }
    })

    historyTable.jTable.addMouseListener(new MouseAdapter {override def mouseClicked(e:MouseEvent) = {
      if (e.getClickCount == 2) doValuation}}
    )

    val negativeHighlighter = new ColorHighlighter(new HighlightPredicate {
      def isHighlighted(renderer:java.awt.Component, adapter:org.jdesktop.swingx.decorator.ComponentAdapter) = {
        adapter.getValue match {
          case v:TableCell => (v.doubleValueIgnoringErrors match {
            case None => false
            case Some(value) => (value < 0.0)
          })
          case _ => false
        }
      }
    })
    negativeHighlighter.setForeground(Color.RED)
    historyTable.jTable.addHighlighter(negativeHighlighter)

    val tradePanel = new MigPanel {
      def clear = removeAll
      def addPanel(p:MigPanel) {
        add(p, "ay top, gapright unrel")
      }
      def addComp(comp:Component, constraints:String) {
        add(comp, constraints)
      }
    }
    updateTradePanel

    val button = new Button("Value")

    add(LabelWithSeparator("Trade " + data.tradeID), "spanx, growx, wrap")
    add(historyTable, "skip 1, pushx, wrap")
    add(tradePanel, "split 2, skip 1")
    add(button, "ay top, gapright " + RightPanelSpace)

    def updateTradePanel {
      val selection = historyTable.jTable.getSelectedRow
      if (selection != -1) {
        tradePanel.clear
        val row = stable.data(selection)
        val panels = SingleTradePageComponent.generateTradePanels(row, fieldDetailsGroups, stable.columns)
        panels.foreach(tradePanel.addPanel)
        tradePanel.addComp(SingleTradePageComponent.generateCostsPanel(costs(selection)), "newline, split, spanx, growx")
        tradePanel.revalidate
        tradePanel.repaint
      }
    }

    def doValuation {
      val selection = historyTable.jTable.getSelectedRow
      if (selection != -1) {
        val row = stable.data(selection)
        val desk = data.desk
        val rowTimeStamp = row(1).asInstanceOf[TableCell].value.asInstanceOf[Timestamp]
        val (deskAndTimestamp, intradaySubgroupAndTimestamp) = desk match {
          case None => {
            data.intradayGroups match {
              case None => (None,None)
              case Some(intradayGroups) => (None,Some((intradayGroups,rowTimeStamp)))
            }
          }
          case Some(d) => {
            val (timestamps, _) = context.localCache.deskCloses(desk).span(_.timestamp >= rowTimeStamp)
            val tradeTimestamp = timestamps.reverse.head
            (Some((d, tradeTimestamp)), None)
          }
        }
        val tradePredicate = TradePredicate(List(), List(List((Field("Trade ID"), SomeSelection(Set(data.tradeID))))))
        val tradeSelection = TradeSelectionWithTimestamp(deskAndTimestamp, tradePredicate, intradaySubgroupAndTimestamp)
        val prl = context.localCache.reportOptionsAvailable.options.filter(_.slidable)
        val initialFieldsState = SingleTradePageComponent.GreeksLayout
        val expiry = data.tradeExpiryDay

        val curveIdentifier = {
          val marketDataSelection = context.getSetting(
            StandardUserSettingKeys.InitialMarketDataSelection,
            MarketDataSelection(context.localCache.pricingGroups(desk).headOption)
          )
          val version = context.localCache.latestMarketDataVersion(marketDataSelection)

          val ci = CurveIdentifierLabel.defaultLabelFromSingleDay(
            MarketDataIdentifier(marketDataSelection, version),
            expiry.exp, context.localCache.ukBusinessCalendar)
          ci.copy(thetaDayAndTime = ci.thetaDayAndTime.copyTimeOfDay(TimeOfDay.EndOfDay))
        }

        val rp = ReportParameters(
          tradeSelection,
          curveIdentifier,
          ReportOptions(prl,None,None),
          expiry.exp,
          None,
          runReports = true)

        context.goTo(new SingleTradeMainPivotReportPage(data.tradeID, row, fieldDetailsGroups, stable.columns, rp,
          PivotPageState.default(initialFieldsState)))
      }
    }
    reactions += {case ButtonClicked(`button`) => doValuation}
    listenTo(button)
  }
  add(mainPanel, "push, grow")
}

case class TradeData(tradeID:TradeIDLabel, tradeHistory:(STable,List[FieldDetailsGroupLabel],List[CostsLabel]), desk:Option[Desk],
                        tradeExpiryDay:TradeExpiryDay, intradayGroups:Option[IntradayGroups]) extends PageData

class SingleTradeMainPivotReportPage(val tradeID:TradeIDLabel, val tradeRow:List[Any], val fieldDetailsGroups:List[FieldDetailsGroupLabel],
                                          val columns:List[SColumn], val reportParameters0:ReportParameters, val pivotPageState0:PivotPageState)
        extends MainPivotReportPage(true, reportParameters0, pivotPageState0) {
  override def toolbarButtons(context:PageContext, data:PageData) = {
    val buttons = super.toolbarButtons(context, data)

    val valuationParametersButton = new ToolBarButton {
      text = "Valuation Parameters"
      icon = StarlingIcons.icon("/icons/16x16_valuation_parameters.png")
      tooltip = "Show the parameters that were used to value this trade"

      reactions += {
        case ButtonClicked(b) => context.goTo(ValuationParametersPage(tradeID, tradeRow, fieldDetailsGroups, columns, reportParameters0))
      }
    }
    valuationParametersButton :: buttons
  }

  override def selfPage(pps:PivotPageState) = new SingleTradeMainPivotReportPage(tradeID, tradeRow, fieldDetailsGroups, columns,
    reportParameters0, pps)
  override def selfReportPage(rp:ReportParameters, pps:PivotPageState) = new SingleTradeMainPivotReportPage(tradeID, tradeRow, fieldDetailsGroups,
    columns, rp, pps)

  override def hashCode = tradeID.hashCode ^ tradeRow.hashCode ^ fieldDetailsGroups.hashCode ^ columns.hashCode ^
          reportParameters0.hashCode ^ pivotPageState0.hashCode
  override def equals(obj:Any) = obj match {
    case other:SingleTradeMainPivotReportPage => {
      tradeID == other.tradeID && tradeRow == other.tradeRow && fieldDetailsGroups == other.fieldDetailsGroups &&
      columns == other.columns && reportParameters0 == other.reportParameters0 && pivotPageState0 == other.pivotPageState0
    }
    case _ => false
  }
}

case class ValuationParametersPage(tradeID:TradeIDLabel, tradeRow:List[Any], fieldDetailsGroups:List[FieldDetailsGroupLabel],
                                   columns:List[SColumn], reportParameters:ReportParameters) extends Page {
  def text = "Valuation Parameters"
  def icon = StarlingIcons.im("/icons/16x16_valuation_parameters.png")
  def build(reader:PageBuildingContext) = {
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
      tradeRow, fieldDetailsGroups, columns, reportParameters)
  }
  def createComponent(context:PageContext, data:PageData, browserSize:Dimension) = {
    new ValuationParametersPageComponent(context, data)
  }
}

case class ValuationParametersPageData(tradeValuation:TradeValuation, tradeRow:List[Any],
                                       fieldDetailsGroups:List[FieldDetailsGroupLabel], columns:List[SColumn],
                                       reportParameters:ReportParameters) extends PageData

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

class ValuationParametersPageComponent(context:PageContext, pageData:PageData) extends MigPanel with PageComponent {
  val data = pageData.asInstanceOf[ValuationParametersPageData]

  val mainPanel = new MigPanel("insets 0") {
    val tradePanels = SingleTradePageComponent.generateTradePanels(data.tradeRow, data.fieldDetailsGroups, data.columns)
    val tradePanel = new MigPanel("insets 0") {
      tradePanels.foreach(add(_, "ay top, gapright unrel"))
    }
    val valuationParametersTable = new TableTable(data.tradeValuation.valuationParameters)
    val valuationParametersTablePanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p]") {
      add(LabelWithSeparator("Valuation Parameters"), "spanx, growx, wrap")
      add(valuationParametersTable, "skip 1, pushx")
    }

    val bottomPanel = new MigPanel("insets 0") {
      add(ValuationParametersPageComponent.reportParametersPanel(data.reportParameters), "gapright unrel, ay top")
      add(valuationParametersTablePanel, "ay top")
    }

    add(tradePanel, "pushx, wrap unrel")
    add(bottomPanel)
  }
  add(mainPanel, "push, grow")
}