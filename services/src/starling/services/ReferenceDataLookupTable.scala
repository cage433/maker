package starling.services

import starling.pivot._


case class ReferenceDataLookupTable(keyName: String, valueName: String, data: Traversable[Tupleable])
  extends UnfilteredPivotTableDataSource {

  val all@List(keyFD, valueFD) = fieldDetails(keyName, valueName)
  def fieldDetailsGroups = List(FieldDetailsGroup("Fields", all))
  override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(keyFD), dataFields = fields(valueFD)))
  def unfilteredData(pfs: PivotFieldsState) = data.toList.map { item => fields(keyFD → item.tuple._1, valueFD → item.tuple._2) }
}