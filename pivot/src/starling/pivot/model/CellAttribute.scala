package starling.pivot.model

object CellAttribute extends Enumeration {
  type CellAttribute = Value
  val Raw = Value("Raw")
  val IntermediateCalculation = Value("IntermediateCalculation")
  val FinalCalculation = Value("FinalCalculation")
}
