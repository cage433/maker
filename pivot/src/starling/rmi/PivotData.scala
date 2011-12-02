package starling.rmi

import starling.pivot.controller.PivotTable
import starling.pivot._

/**
 * The object that is sent back to the client
 */
case class PivotData(
  allFields:List[Field],
  fieldGroups:List[FieldGroup],
  dataFields:Set[Field],
  pivotFieldsState:PivotFieldsState,
  drillDownGroups:List[DrillDownInfo],
  pivotTable:PivotTable,
  // available pages include Vols and Correlations
  availablePages:List[String],
  defaultPivotState:Option[DefaultPivotState],
  reportSpecificOptions : List[(String, List[Any])]
) {
}