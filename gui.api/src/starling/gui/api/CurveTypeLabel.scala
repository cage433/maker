package starling.gui.api

case class CurveTypeLabel(name:String)

case class CurveLabel(curveType: CurveTypeLabel, marketDataIdentifier: MarketDataIdentifier,
                      environmentSpecification: EnvironmentSpecificationLabel) {
  def copyVersion(version: Int) = copy(marketDataIdentifier = marketDataIdentifier.copyVersion(version))
  def copySelection(selection: MarketDataSelection) = copy(marketDataIdentifier = marketDataIdentifier.copy(selection = selection))

  def observationDay = environmentSpecification.observationDay
}

