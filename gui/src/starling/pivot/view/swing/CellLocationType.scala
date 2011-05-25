package starling.pivot.view.swing

/**
 * Indicates what type of location the cell is at
 */
object CellLocationType extends Enumeration {
  type CellLocationType = Value
  val DataField = Value("DataField")
  val RowAxis = Value("RowAxis")
  val ColAxis = Value("ColAxis")
  val DataValue = Value("DataValue")
}
