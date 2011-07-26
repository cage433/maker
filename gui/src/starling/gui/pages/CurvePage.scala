package starling.gui.pages

import starling.pivot.view.swing.MigPanel
import starling.gui._
import api._
import swing._
import starling.daterange.Day
import starling.rmi.StarlingServer
import starling.pivot.PivotEdits


case class CurvePage(curveLabel: CurveLabel, pivotPageState: PivotPageState) extends AbstractPivotPage(pivotPageState) {
  def marketDataIdentifier = curveLabel.marketDataIdentifier

  def text = "Curve Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_curve_viewer.png")
  override def layoutType = Some("Curve")

  override def refreshFunctions = marketDataIdentifier match {
    case MarketDataIdentifier(MarketDataSelection(pricingGroup, name), SpecificMarketDataVersion(_)) => {
      PricingGroupMarketDataUpdate.matching(pricingGroup).andThen(update => copyVersion(update.version)) ::
      ExcelMarketDataUpdate.matching(name).andThen(update => copyVersion(update.version)) ::
      Nil
    }
    case _ => Nil
  }

  private def copyVersion(version: Int) = copy(curveLabel = curveLabel.copyVersion(version))
  private def copySelection(selection: MarketDataSelection) = copy(curveLabel = curveLabel.copySelection(selection))
  private def copyEnv(envSpec: EnvironmentSpecificationLabel) = copy(curveLabel = curveLabel.copy(environmentSpecification = envSpec))

  def selfPage(pivotPageState: PivotPageState, edits:PivotEdits) = copy(pivotPageState = pivotPageState)

  def dataRequest(pageBuildingContext: PageBuildingContext) = {
    pageBuildingContext.starlingServer.curvePivot(curveLabel, pivotPageState.pivotFieldParams)
  }

  override def configPanel(pageContext: PageContext, data:PageData) = {
    val marketDataSelectionPanel = new MarketDataSelectionComponent(pageContext, None, marketDataIdentifier.selection)
    val marketDataSelectionPanelPanel = new MigPanel {
      border = RoundedBorder(colour = GuiUtils.PivotTableBackgroundColour)
      add(marketDataSelectionPanel, "push, grow")
    }
    val curveTypeChooser = new CurveTypeChooser(pageContext, curveLabel.curveType)
    val curveTypeChooserPanel = new MigPanel {
      border = RoundedBorder(colour = GuiUtils.PivotTableBackgroundColour)
      add(new Label("Curve Type:"))
      add(curveTypeChooser, "push, grow")
    }
    val environmentRules = pageContext.localCache.environmentRulesForPricingGroup(marketDataIdentifier.selection.pricingGroup)
    val envSpecChooser = new EnvironmentSpecificationLabelChooser(curveLabel.environmentSpecification, environmentRules)
    val envSpecChooserPanel = new MigPanel {
      border = RoundedBorder(colour = GuiUtils.PivotTableBackgroundColour)
      add(new Label("Environment Spec:"))
      add(envSpecChooser, "push, grow")
    }

    def latest(curvePage:CurvePage) = {
      val version = pageContext.localCache.latestMarketDataVersion(curvePage.marketDataIdentifier.selection)
      curvePage.copy(curveLabel=curvePage.curveLabel.copyVersion(version))
    }

    curveTypeChooser.reactions += {
      case CurveTypeChangedEvent(selected) => pageContext.goTo(latest(copy(curveLabel=curveLabel.copy(curveType=selected))))
    }

    marketDataSelectionPanel.reactions += {
      case MarketDataSelectionChanged(selection) => { pageContext.goTo(latest(copySelection(selection)), false) }
    }

    def updatePopulatedDays {
      envSpecChooser.flagged = pageContext.localCache.populatedDays(marketDataIdentifier.selection).toSet
    }

    updatePopulatedDays
    envSpecChooser.listenTo(pageContext.remotePublisher)

    envSpecChooser.dayChooser.reactions += {
      case ExcelObservationDay(_, _) | PricingGroupObservationDay(_, _) => updatePopulatedDays
    }

    envSpecChooser.reactions += {
      case EnvironmentSpecificationLabelChangedEvent(ruleChooser, envSpec) => pageContext.goTo(latest(copyEnv(envSpec)))
    }

    val configPanel = new MigPanel with ConfigPanel {
      add(marketDataSelectionPanelPanel, "wrap")
      add(curveTypeChooserPanel, "split, spanx")
      add(envSpecChooserPanel)

      def displayName = "Curve Selection"
    }

    Some(ConfigPanels(List(configPanel), new Label(""), Action("BLA"){}))
  }

  override def bookmark(server:StarlingServer):Bookmark = {
    CurveBookmark(curveLabel.curveType, curveLabel.environmentSpecification.environmentRule,
      curveLabel.marketDataIdentifier.selection, pivotPageState)
  }
}

case class CurveBookmark(curveType:CurveTypeLabel, envRuleLabel:EnvironmentRuleLabel, selection:MarketDataSelection, pivotPageState:PivotPageState) extends Bookmark {
  def daySensitive = true
  def createPage(day0:Option[Day], server:StarlingServer, context:PageContext) = {
    val day = day0.get
    val newEnvironmentSpecification = EnvironmentSpecificationLabel(day, envRuleLabel)
    val newMarketDataIdentifier = server.latestMarketDataIdentifier(selection)
    val newCurveLabel = CurveLabel(curveType, newMarketDataIdentifier, newEnvironmentSpecification)
    CurvePage(newCurveLabel, pivotPageState)
  }
}