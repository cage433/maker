package starling.varcalculator


import cern.colt.matrix.{DoubleMatrix1D => Vector}
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.quantity.{UOM, Quantity}
import starling.utils.conversions.RichColtMatrices._
import starling.quantity.UOM.USD
import starling.instrument.Instrument


/** 
 * A collection of Instrument, VarScenarioValues pairs. Instruments can be repeated, unlike in VarUtpResults. Used
 * when we want the var of a collection of (possibly non-unique) instruments.
 */
case class VarInstrumentResults private (
  val scenValuesList : List[(Instrument, VarScenarioValues)],
  val positionList : List[(Instrument, MarketPositions)],
  val usdPositionList : List[(Instrument, Quantity)],
  instErrors : List[(Instrument, Throwable)]
)
extends VarResults
{
  def this() = this(
    List[(Instrument, VarScenarioValues)](),
    List[(Instrument, MarketPositions)](),
    List[(Instrument, Quantity)](),
    List[(Instrument, Throwable)]())

  val scenarioValues = scenValuesList.map(_._2)
  val marketPositions = positionList.map(_._2)
  val usdPositions = usdPositionList.map(_._2)
  val errors = instErrors
  
  def +( x : (Instrument, VarScenarioValues, MarketPositions, Quantity)) : VarInstrumentResults =
    VarInstrumentResults(
      (x._1, x._2) :: scenValuesList,
      (x._1, x._3) :: positionList,
      (x._1, x._4) :: usdPositionList,
      instErrors
    )
  def +(inst : Instrument, t : Throwable) : VarInstrumentResults = {
    VarInstrumentResults(scenValuesList, positionList, usdPositionList, (inst, t) :: instErrors)
  }

  @transient lazy val scenValuesMap = Map.empty ++ scenValuesList
  def VaR(inst : Instrument, centile : Double) : Double = scenValuesMap(inst).VaR(centile)

}


object VarInstrumentResults{
  def build(scenGen : ScenarioGenerator, allInstruments : List[Instrument], nScenarios : Int) : VarInstrumentResults = {
    val valuationCCY = USD
    var errors = List[(Instrument, Throwable)]()
    var mtms = Map[Instrument, Quantity]()
    val originalBucketedEnv = scenGen.originalBucketedEnv
    allInstruments.foreach{
      inst =>
        try {
          val mtm = inst.mtm(originalBucketedEnv, valuationCCY)
          mtms = mtms ++ Map(inst -> mtm)
        } catch {
          case e => {
            errors = errors ++ Map(inst -> e)
          }
        }
    }
    val goodInstruments = allInstruments.filterNot(errors.map(_._1).contains)
    val mtmVector =  new DVector(goodInstruments.map(mtms(_).checkedValue(valuationCCY)).toArray)
    val nInstruments = goodInstruments.size

    val resultMatrix = new DMatrix(nScenarios, nInstruments)
    for (iScenario <- 0 until nScenarios){
      val env = scenGen.next
      resultMatrix(iScenario) := goodInstruments.map(_.mtm(env, valuationCCY).checkedValue(valuationCCY))
    }
    for (iScenario <- 0 until nScenarios){
      resultMatrix(iScenario) -= mtmVector
    }
    goodInstruments.zipWithIndex.foldLeft(
      new VarInstrumentResults(List[(Instrument, VarScenarioValues)](), List[(Instrument, MarketPositions)](), List[(Instrument, Quantity)](), errors)
    ){
      case (vr, (inst, iCol)) =>
        vr + (inst,
              VarScenarioValues(resultMatrix.viewColumn(iCol), Set()),
              inst.parallelShiftPositions(originalBucketedEnv),
              inst.usdDeltaPosition(originalBucketedEnv))
    }
  }
}