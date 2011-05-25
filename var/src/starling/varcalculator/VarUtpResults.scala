package starling.varcalculator


import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.instrument._
import starling.quantity.Quantity


/**
 *  A Map of UTP into VaR scenario values
 */
case class VarUtpResults private (
  utpScenValues : Map[Instrument, VarScenarioValues],
  utpPositions : Map[Instrument, MarketPositions],
  utpUsdPositions : Map[Instrument, Quantity],
  val utpErrors : Map[Instrument, Throwable]
)
  extends VarResults
{
  def this() = this(Map.empty[Instrument, VarScenarioValues], Map.empty[Instrument, MarketPositions], Map.empty[Instrument, Quantity], Map.empty[Instrument, Throwable])

  val scenarioValues = utpScenValues.valuesIterator.toSeq

  val marketPositions = utpPositions.valuesIterator.toSeq
  val usdPositions = utpUsdPositions.valuesIterator.toSeq
  
  val errors = utpErrors.iterator.toSeq

  def +( x : (Instrument, VarScenarioValues, MarketPositions, Quantity)) : VarUtpResults = {
    val (inst, scenarioValues, marketPositions, usdPosition) = x
    assert(! utpScenValues.contains(inst) && ! errors.contains(inst), "Adding a UTP we already have")
    VarUtpResults(
      utpScenValues ++ Map(inst -> scenarioValues),
      utpPositions ++ Map(inst -> marketPositions),
      utpUsdPositions ++ Map(inst -> usdPosition),
      utpErrors
    )
  }

  def +(inst : Instrument, t : Throwable) : VarUtpResults = {
    assert(! utpScenValues.contains(inst) && ! utpErrors.contains(inst), "Adding a UTP we already have")
    VarUtpResults(utpScenValues, utpPositions, utpUsdPositions, utpErrors ++ Map(inst -> t))
  }

  def instScenValues(inst : Instrument) = utpScenValues(inst)

  def instMarketPositions(inst : Instrument) = utpPositions(inst)
  def instUsdPositions(inst : Instrument) = utpUsdPositions(inst)

  def hasAnyError(insts : Iterable[Instrument]) : Option[(Instrument, Throwable)] = {
    insts.find(utpErrors.contains(_)) match {
      case Some(inst) => Some((inst, utpErrors(inst)))
      case None => None
    }
  }

  def scenaraiosFor(instrument:Instrument):Either[VarScenarioValues,Throwable] = utpScenValues.get(instrument) match {
    case Some(values) => Left(values)
    case None => {
      utpErrors.get(instrument) match {
        case Some(t) => Right(t)
        case None => Right(new Exception("No var or exception found"))
      }
    }
  }
}


object VarUtpResults{
  def build(scenGen : ScenarioGenerator, instruments : List[Instrument], nScenarios : Int) : VarUtpResults = {
    val dups = instruments filterNot ((Set() ++ instruments).toList contains)
    require(dups.isEmpty, "Duplicate utp instruments: "+ dups)
    val instrumentResults = VarInstrumentResults.build(scenGen, instruments, nScenarios)
    VarUtpResults(
      Map.empty ++ instrumentResults.scenValuesList,
      Map.empty ++ instrumentResults.positionList,
      Map.empty ++ instrumentResults.usdPositionList,
      Map.empty ++ instrumentResults.instErrors)
  }

  def utpPortfolioScenarioValues(utpPortfolio :UTP_Portfolio, utpResults: VarUtpResults): VarScenarioValues = {
    var results = new VarScenarioValues(utpResults.nScenarios)
    utpPortfolio.portfolio.view.foreach {
      case (inst, volume) =>

        results = results + (utpResults.instScenValues(inst) * volume)
    }
    results
  }
  def utpPortfolioMarketPositions(utpPortfolio :UTP_Portfolio, utpResults: VarUtpResults): MarketPositions = {
    var results = new MarketPositions
    utpPortfolio.portfolio.view.foreach {
      case (inst, volume) =>

        results = results + (utpResults.instMarketPositions(inst) * volume)
    }
    results
  }
  def utpPortfolioUsdPosition(utpPortfolio :UTP_Portfolio, utpResults: VarUtpResults): Quantity = {
    var result = Quantity.NULL
    utpPortfolio.portfolio.view.foreach {
      case (inst, volume) =>

        result = result + (utpResults.instUsdPositions(inst) * volume)
    }
    result
  }

}
