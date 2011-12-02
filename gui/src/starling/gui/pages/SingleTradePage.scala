package starling.gui.pages

import starling.gui.api._
import starling.gui._
import starling.browser.common.GuiUtils._
import javax.swing.ListSelectionModel
import starling.pivot._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import starling.tradestore.TradePredicate
import org.jdesktop.swingx.decorator.{ColorHighlighter, HighlightPredicate}
import java.awt.event.{MouseEvent, MouseAdapter}
import collection.mutable.ListBuffer
import collection.immutable.TreeMap
import starling.quantity.Quantity
import starling.daterange.{Day, TimeOfDay, Timestamp}
import org.jdesktop.swingx.renderer.{DefaultTableRenderer, LabelProvider, StringValue}
import swing.{Alignment, Component, Label}
import starling.gui.StarlingLocalCache._
import starling.browser.common.{ButtonClickedEx, NewPageButton, MigPanel}
import starling.browser._
import java.awt.{Dimension, Color}
import starling.utils.{Log, SColumn, STable}

case class SingleTradePage(tradeID:TradeIDLabel, desk:Option[Desk], tradeExpiryDay:TradeExpiryDay, intradayGroups:Option[IntradayGroups]) extends StarlingServerPage {
  Log.info("Created single trade page")
  def text = "Trade " + tradeID
  def icon = StarlingIcons.im("/icons/tablenew_16x16.png")
  def build(reader:StarlingServerContext) = TradeData(tradeID, reader.tradeService.readTradeVersions(tradeID), desk, tradeExpiryDay, intradayGroups)
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new SingleTradePageComponent(context, data)
}

object SingleTradePageComponent {
  def generateTradePanels(tradeRow:List[Any], fieldDetailsGroups:List[FieldDetailsGroupLabel], columns:List[SColumn]) = {
    val columnIndexMap = Map() ++ columns.map(_.name).zipWithIndex
    val panelBuffer = new ListBuffer[(String, MigPanel)]
    for (group <- fieldDetailsGroups) {
      val groupPanel = new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][fill,grow]") {
        add(LabelWithSeparator(group.groupName), "spanx, growx, wrap")
        for (fieldName <- group.childNames) {
          if (columnIndexMap.contains(fieldName)) {
            add(new Label(fieldName) {foreground = Color.BLUE}, "ay top, skip 1")
            val value = tradeRow(columnIndexMap(fieldName)).asInstanceOf[TableCell]
            val valueLabel = new Label {
              xAlignment = Alignment.Left
              if (fieldName == "Strategy") {
                // Make a special case for strategy so that it doesn't take up too much horizontal space.
                val paths = value.value.toString.split("/")
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
      panelBuffer += ((group.groupName, groupPanel))
    }
    panelBuffer.toList
  }

  def generateCostsPanel(costs:CostsLabel) = {
    new MigPanel("insets 0", "[" + StandardLeftIndent + "][p][p]") {
      val columns = List(SColumn("Cost Type"), SColumn("Value"))
      class MyPivotQuantity(val value:Quantity) extends PivotQuantity(Map(value.uom -> value.value), Map[String,List[StackTrace]]()) {
        override def toString = PivotFormatter.shortAndLongText(this, PivotFormatter.DefaultExtraFormatInfo)._1
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
      negativeHighlighter.setSelectedForeground(Color.RED)
      costsChooser.jTable.addHighlighter(negativeHighlighter)
      costsChooser.jTable.getSelectionModel.addListSelectionListener(new ListSelectionListener {
        def valueChanged(e:ListSelectionEvent) {
          if (!e.getValueIsAdjusting) {
            updateInfoPanel(costsChooser.jTable.getSelectedRow)
          }
        }
      })
      val infoPanel = new MigPanel("insets 0") {
        def update(comp:Component) {
          removeAll
          add(comp, "push,grow")
          revalidate()
          repaint()
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

  import ReportSpecificOptions._
  val DefaultReportSpecificChoices = TreeMap(
    lotsLabel -> false,
    positionLabel-> "Default",
    tenorLabel -> "M",
    futuresAsSwapsLabel-> false,
    showEqFuturesLabel-> false,
    useSkewLabel-> true,
    futuresAsSpreadsLabel-> false,
    atmVegaLabel-> false,
    collapseOptionsLabel-> true)

  val GreeksLayout = DefaultPivotState(new PivotFieldsState(
    rowFields=List(
      Field(tradeIDLabel),
      Field(riskMarketLabel),
      Field(riskPeriodLabel)
    ),
    columns = ColumnTrees(List(
    ColumnTree(Field("Instrument"), false,
      List("Position", "Market Price", "Initial Price", "P&L", "Quantity").map(f=>ColumnTree(Field(f), true)): _*)
    )),
    reportSpecificChoices = DefaultReportSpecificChoices
  ))
}

class SingleTradePageComponent(context:PageContext, pageData:PageData) extends MigPanel("insets 0") with PageComponent {
  val data = pageData.asInstanceOf[TradeData]
  val (stable, fieldDetailsGroups, costs) = data.tradeHistory
  val columnIndexMap = Map() ++ stable.columns.zipWithIndex.map{case (col,index) => (col.name,index)}

  val mainPanel = new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
    val historyTable = new TableTable(stable)
    val jTable = historyTable.jTable
    jTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    val heightToUse = if (jTable.getPreferredSize.height < 150) 150 else jTable.getPreferredScrollableViewportSize.height
    jTable.setPreferredScrollableViewportSize(new Dimension(jTable.getPreferredSize.width, heightToUse))
    val rowToSelect = stable.data.size - 1
    jTable.setRowSelectionInterval(rowToSelect, rowToSelect)
    jTable.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e:ListSelectionEvent) {
        if (!e.getValueIsAdjusting) {
          updateTradePanel()
        }
      }
    })

    jTable.addMouseListener(new MouseAdapter {override def mouseClicked(e:MouseEvent) {
      if (e.getClickCount == 2) doValuation(Modifiers.modifiersEX(e.getModifiersEx))}}
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
    negativeHighlighter.setSelectedForeground(Color.RED)
    jTable.addHighlighter(negativeHighlighter)

    val stringValue = new StringValue {
      def getString(value:AnyRef) = {
        value match {
          case t:TableCell => t.text
          case other => other.toString
        }
      }
    }
    val provider = new LabelProvider(stringValue)
    jTable.setDefaultRenderer(classOf[Object], new DefaultTableRenderer(provider))

    val tradePanel = new MigPanel {
      def clear() {removeAll}
      def addPanel(p:MigPanel) {
        add(p, "ay top, gapright unrel")
      }
      def addComp(comp:Component, constraints:String) {
        add(comp, constraints)
      }
    }
    updateTradePanel()

    val button = new NewPageButton {
      text = "Value"
    }

    add(LabelWithSeparator("Trade " + data.tradeID), "spanx, growx, wrap")
    add(historyTable, "skip 1, pushx, wrap")
    add(tradePanel, "split 2, skip 1")
    add(button, "ay top, gapright " + RightPanelSpace)

    def updateTradePanel() {
      val selection = jTable.getSelectedRow
      if (selection != -1) {
        tradePanel.clear()
        val row = stable.data(selection)
        val panels = SingleTradePageComponent.generateTradePanels(row, fieldDetailsGroups, stable.columns)
        panels.map(_._2).foreach(tradePanel.addPanel)
        // Only show costs panel if there are any.
        if (costs.head.costInfo.nonEmpty) {
          tradePanel.addComp(SingleTradePageComponent.generateCostsPanel(costs(selection)), "newline, split, spanx, growx")
        }
        tradePanel.revalidate()
        tradePanel.repaint()
      }
    }

    def doValuation(modifiers:Modifiers) {
      val selection = jTable.getSelectedRow
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
            if(jTable.getRowCount - 1 == selection) {
              // we've selected the most up to date
               val tradeTimestamp = context.localCache.deskCloses(desk).sortWith(_.timestamp >= _.timestamp).head
              (Some((d, tradeTimestamp)), None)
            } else {
              val (timestamps, _) = context.localCache.deskCloses(desk).span(_.timestamp >= rowTimeStamp)
              val tradeTimestamp = timestamps.reverse.head
              (Some((d, tradeTimestamp)), None)
            }
          }
        }
        val tradePredicate = TradePredicate(List(), List(List((Field("Trade ID"), SomeSelection(Set(data.tradeID))))))
        val tradeSelection = TradeSelectionWithTimestamp(deskAndTimestamp, tradePredicate, intradaySubgroupAndTimestamp)
        val prl = context.localCache.reportOptionsAvailable.options.filter(_.slidable)
        val initialFieldsState = SingleTradePageComponent.GreeksLayout
        val expiry = data.tradeExpiryDay.exp.startOfFinancialYear

        val curveIdentifier = {
          val deskPricingGroups = context.localCache.pricingGroups(desk)
          val pricingGroup = deskPricingGroups.headOption
          val marketDataSelection = {
            val tmp = context.getSetting(StandardUserSettingKeys.InitialMarketDataSelection, MarketDataSelection(pricingGroup))
            if (deskPricingGroups.contains(tmp.pricingGroup)) {
              tmp
            } else {
              MarketDataSelection(pricingGroup)
            }
          }

          val version = context.localCache.latestMarketDataVersion(marketDataSelection)

          val enRule = pricingGroup match {
            case Some(pg) if pg == PricingGroup.Metals => EnvironmentRuleLabel.AllCloses
            case _ => EnvironmentRuleLabel.COB
          }

          val ci = CurveIdentifierLabel.defaultLabelFromSingleDay(
            MarketDataIdentifier(marketDataSelection, version),
            context.localCache.ukBusinessCalendar)
          ci.copy(thetaDayAndTime = ci.thetaDayAndTime.copyTimeOfDay(TimeOfDay.EndOfDay), environmentRule = enRule)
        }

        val rp = ReportParameters(
          tradeSelection,
          curveIdentifier,
          ReportOptions(prl,None,None),
          expiry,
          None,
          runReports = true)

        context.goTo(new SingleTradeMainPivotReportPage(data.tradeID, rp, PivotPageState.default(initialFieldsState)), modifiers)
      }
    }
    reactions += {case ButtonClickedEx(`button`, e) => doValuation(Modifiers.modifiers(e.getModifiers))}
    listenTo(button)
  }
  add(mainPanel, "push, grow")
}

case class TradeData(tradeID:TradeIDLabel, tradeHistory:(STable,List[FieldDetailsGroupLabel],List[CostsLabel]), desk:Option[Desk],
                        tradeExpiryDay:TradeExpiryDay, intradayGroups:Option[IntradayGroups]) extends PageData

class SingleTradeMainPivotReportPage(val tradeID:TradeIDLabel, val reportParameters0:ReportParameters, val pivotPageState0:PivotPageState)
        extends MainPivotReportPage(true, reportParameters0, pivotPageState0) {
  override def toolbarButtons(context:PageContext, data:PageData) = {
    val buttons = super.toolbarButtons(context, data)

    val valuationParametersButton = new NewPageToolBarButton {
      text = "Valuation Parameters"
      val leftIcon = StarlingIcons.im("/icons/16x16_valuation_parameters.png")
      tooltip = "Show the parameters that were used to value this trade"

      reactions += {
        case ButtonClickedEx(b, e) => context.goTo(ValuationParametersPage(tradeID, reportParameters0, ReportSpecificChoices()), Modifiers.modifiers(e.getModifiers))
      }
    }
    valuationParametersButton :: buttons
  }
  override def selfPage(pps:PivotPageState, edits:PivotEdits) = new SingleTradeMainPivotReportPage(tradeID, reportParameters0, pps)
  override def selfReportPage(rp:ReportParameters, pps:PivotPageState) = new SingleTradeMainPivotReportPage(tradeID, rp, pps)
  override def hashCode = tradeID.hashCode ^ reportParameters0.hashCode ^ pivotPageState0.hashCode
  override def equals(obj:Any) = obj match {
    case other:SingleTradeMainPivotReportPage => {
      tradeID == other.tradeID && reportParameters0 == other.reportParameters0 && pivotPageState0 == other.pivotPageState0
    }
    case _ => false
  }
  override def bookmark(serverContext:StarlingServerContext, pd:PageData):Bookmark = {
    SingleTradeReportBookmark(tradeID, serverContext.reportService.createUserReport(reportParameters0), pivotPageState)
  }
}

case class SingleTradeReportBookmark(tradeID:TradeIDLabel, data:UserReportData, pps:PivotPageState) extends StarlingBookmark {
  def daySensitive = true
  def createStarlingPage(day:Option[Day], serverContext:StarlingServerContext, context:PageContext) = {
    day match {
      case None => throw new Exception("We need a day")
      case Some(d) => {
        val reportParameters = serverContext.reportService.createReportParameters(data, d)
        new SingleTradeMainPivotReportPage(tradeID, reportParameters, pps)
      }
    }
  }
}
