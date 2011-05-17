package starling.varcalculator

import starling.quantity.UOM._
import starling.quantity.Quantity


trait VarResults{
  val scenarioValues : Seq[VarScenarioValues]
  val marketPositions : Seq[MarketPositions]
  val usdPositions : Seq[Quantity]

  lazy val nScenarios : Int = {
    val n = if (scenarioValues.isEmpty)
        0
      else
        scenarioValues.head.nScenarios
    // Assertion can't go outside this block otherwise it gets called before the subclasses scenario values are initialised
    assert(scenarioValues.forall(_.nScenarios == n), "Scenario size mismatch")
    n
  }

  lazy val netScenarioValues : VarScenarioValues = {
    (new VarScenarioValues(nScenarios) /: scenarioValues)(_+_)
  }
  lazy val netMarketPositions : MarketPositions = {
    (new MarketPositions /: marketPositions)(_+_)
  }

  def VaR(centile : Double) = Quantity(netScenarioValues.VaR(centile), USD)
  def CVaR(centile : Double) = Quantity(netScenarioValues.CVaR(centile), USD)
  def standardError(centile : Double) = Quantity(netScenarioValues.standardError(centile), USD)
  def standardError = Quantity(netScenarioValues.standardError, USD)

  def netPosition = Quantity.sum(marketPositions.map(_.netPosition))
  def netUSDPosition = Quantity.sum(usdPositions)
}