package starling.services

import starling.pivot._

case class ReferenceDataLookupTable(keyName: String, valueName: String, data: Map[_, _])
  extends UnfilteredPivotTableDataSource {

  val all@List(keyFD, valueFD) = fieldDetails(keyName, valueName)
  def fieldDetailsGroups = List(FieldDetailsGroup("Fields", all))
  override val initialState = PivotFieldsState(rowFields = fields(keyFD), dataFields = fields(valueFD))
  def unfilteredData(pfs: PivotFieldsState) = data.toList.map { case (key, value) => fields(keyFD → key, valueFD → value) }
}