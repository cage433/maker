package starling.gui.pages

import starling.pivot.view.swing.MigPanel
import starling.gui._
import api._
import swing._


case class CurvePage(curveLabel: CurveLabel, pivotPageState: PivotPageState) extends AbstractPivotPage(pivotPageState) {
  val curveType = curveLabel.curveType
  val environmentSpecification = curveLabel.environmentSpecification
  val marketDataIdentifier = curveLabel.marketDataIdentifier

  def text = "Curve Viewer"
  override val icon = StarlingIcons.im("/icons/16x16_curve_viewer.png")
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

  def selfPage(pivotPageState: PivotPageState) = copy(pivotPageState = pivotPageState)

  def dataRequest(pageBuildingContext: PageBuildingContext) = {
    pageBuildingContext.starlingServer.curvePivot(curveLabel, pivotPageState.pivotFieldParams)
  }

  override def configPanel(pageContext: PageContext, data:PageData) = {
    val environmentRules = pageContext.localCache.environmentRulesForPricingGroup(marketDataIdentifier.selection.pricingGroup)
    val marketDataSelectionPanel = new MarketDataSelectionComponent(pageContext, None, marketDataIdentifier.selection)
    val envSpecChooser = new EnvironmentSpecificationLabelChooser(environmentSpecification, environmentRules)
    val curveTypeChooser = new CurveTypeChooser(pageContext, curveType)

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

    val configPanel = new MigPanel("gapx unrel") with ConfigPanel {
      add(marketDataSelectionPanel)
      add(curveTypeChooser)
      add(envSpecChooser)

      def displayName = "Curve"
    }

    Some(ConfigPanels(List(configPanel), new Label(""), Action("BLA"){}))
  }
}