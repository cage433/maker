package starling.reports.impl.pivot

import starling.pivot._
import model.UndefinedValue
import scala.collection.mutable.{Map => MutableMap}
import collection.Seq
import collection.immutable.TreeMap
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}
import starling.gui.api.Desk

/**
 * Joins a list of PivotReportData to a PivotTableDataSource (which must have InstrumentID and UTPVolume fields)
 */
class ReportField(val name:String, val pivotReport:PivotReportData[_ <: PivotReportRow], val pivotReportField:PivotReportField[_]) {
  def fieldDetails:FieldDetails = pivotReportField.pivotFieldDetails
}
class ReportPivotTableDataSource(tradePivotTable:PivotTableDataSource, reports:List[PivotReportData[_ <: PivotReportRow]], desk : Option[Desk]) extends PivotTableDataSource {
  type FieldValues = Seq[Any]
  type FieldBindings = scala.collection.Map[Field, Any]

  val instrumentField = Field(Field.instrumentID_str)
  val utpVolumeField = Field(Field.utpVolume_str)

  val reportFieldTuples = reports.flatMap(report=>createReportFields(report))
  val reportFieldDetailsByField:Map[Field,FieldDetails] = Map() ++ (for ((field,reportField) <- reportFieldTuples) yield field->reportField.fieldDetails)
  val reportFieldsByField = reportFieldTuples.map(_._2).groupBy(rf=>Field(rf.name))
  val reportFields:List[Field] = reportFieldTuples.map{_._1}.distinct
  val tradeFields = Set() ++ tradePivotTable.fieldDetails.map{f=>f.field}
  private val tmpReportFields = reportFields.map(f=>reportFieldDetailsByField(f)).toSet.toList
  private val tmpTradeFields = tradePivotTable.fieldDetails.filter(f=>f.field!=instrumentField&&f.field!=utpVolumeField).toList
//  val fields = tmpReportFields ::: tmpTradeFields

  val fieldDetailsGroups = FieldDetailsGroup("Report Fields", tmpReportFields) :: tradePivotTable.fieldDetailsGroups.filterNot(_.name == "HiddenUTPFields")

  private val fieldToFieldDetails : Map[Field, FieldDetails]= Map() ++ tradePivotTable.fieldDetails.map(f=>f.field->f)

  private val allFieldToFieldDetails : Map[Field, FieldDetails]= Map() ++ fieldDetails.map(f=>f.field->f)

  private val fieldsByName = Map() ++ fieldDetails.map(f=>f.field.name->f.field)
  def namesToFields(fieldNames:List[String]) = fieldNames.flatMap(fieldsByName.get(_) match { case Some(f) => List(f); case None=>List()})

  private def createReportFields(pivotReport:PivotReportData[_ <: PivotReportRow]):List[(Field,ReportField)] = {
    pivotReport.fields.map(reportField => Field(reportField.name)->new ReportField(reportField.name, pivotReport, reportField.field))
  }

  override def drillDownGroups() = tradePivotTable.drillDownGroups

  val tradeResult = tradePivotTable.data(PivotFieldsState())

  /**
   * Collects the combined data values for each row/column grouping. The groupings
   * are a partition of the trade and report data.
   * Two steps
   *    1.  For each trade collect both the combined values, and also
   *        maintain a map of UTP -> Volume for each grouping.
   *    2.  Pass the UTP -> Volume map to each report - from this the report returns
   *        its own combined values which are then collected together with the trade combined values for that group
   */
  case class GroupedAndCombinedFieldBindingsBuilder(
    tradeFilters          : FiltersList,
    tradeFieldsToGroupBy  : Seq[Field],
    tradeFieldsToCombine  : Seq[Field],
    reportSpecificOptions : Map[String, Any]
  )
  {
    type VolumeMap = MutableMap[UTPIdentifier, Double]
    val reportSpecificChoices = ReportSpecificChoices.create(reportSpecificOptions)
    private val nullTradeDataCombinedValues : FieldValues= tradeFieldsToCombine.map(fieldToFieldDetails(_).nullGroup)
    private val groupCombinedValuesMap =  MutableMap[FieldValues, MutableMap[UTPIdentifier, (Double, FieldValues)]]()

    private def rowMatchesSelection(row: FieldBindings): Boolean = {
      tradeFilters.exists(_.forall {case (field, selection) => selection.matches(allFieldToFieldDetails(field), row.getOrElse(field, UndefinedValue))})
    }

    private def incrementUtpVolumeAndCombineTradeFieldValues(groupValues: FieldValues, row : FieldBindings){
      val utpVolumesAndCombinedValues = groupCombinedValuesMap.getOrElseUpdate(groupValues, MutableMap[UTPIdentifier, (Double, FieldValues)]())
      val utpId: UTPIdentifier = row(instrumentField).asInstanceOf[UTPIdentifier]
      val volume: Double = row(utpVolumeField).asInstanceOf[Double]
      val (currentVolume, currentCombinedTradeFieldValues) = utpVolumesAndCombinedValues.getOrElseUpdate(utpId, (0.0, nullTradeDataCombinedValues))
      val newVolume = currentVolume + volume


      val newCombinedTradeFieldValues : FieldValues = tradeFieldsToCombine.zip(currentCombinedTradeFieldValues).map{
        case (field, currentCombinedValue) => {
          row.get(field) match {
            case None => currentCombinedValue
            case Some(newValue) => fieldToFieldDetails(field).combine(currentCombinedValue, newValue)
          }
        }
      }
      utpVolumesAndCombinedValues(utpId) = (newVolume, newCombinedTradeFieldValues)
    }

    // Step 1.
    def +=(row: FieldBindings) {
      if (rowMatchesSelection(row)) {
        val groupValues = tradeFieldsToGroupBy.map{
           field  =>
             val fieldDetails = fieldToFieldDetails(field)
             fieldDetails.transformValueForGroupByField(row.getOrElse(field, UndefinedValue))
        }
        incrementUtpVolumeAndCombineTradeFieldValues(groupValues, row)
//        combineNewValues(groupValues, row)
      }
    }

    // Setp 2.
    def collectMergedReportValues: List[Map[Field, Any]] = {
      var result = List[Map[Field, Any]]()
      for (
        (groupedValues, utpVolumes) <- groupCombinedValuesMap;
        groupedTradeBindings = tradeFieldsToGroupBy.zip(groupedValues).toMap;
        report <- reports
      ){
        // This should really by inlined - but I got strange type compilation errors when I tried.
        def iterateOverReportRows[R <: PivotReportRow](report: PivotReportData[R], utpIdVolumes: MutableMap[UTPIdentifier, (Double, FieldValues)]) {


          for (row <- report.mergedResult(utpIdVolumes, reportSpecificChoices)) {
            val reportRowValues: Map[Field, Any] = report.fields.flatMap {
              field => {
                val value = field.value(row)
                if (value != field.pivotFieldDetails.nullValue) {
                  Some(field.pivotFieldDetails.field -> value)
                } else {
                  None
                }
              }
            }.toMap
            val combinedTradeBindings = row match {
              case (_, tradeFieldValues) => tradeFieldsToCombine.zip(tradeFieldValues).toMap
            }
            val newTerm = groupedTradeBindings ++ combinedTradeBindings ++ reportRowValues
            result = newTerm :: result
          }
        }
        iterateOverReportRows(report, utpVolumes)
      }

      result
    }

  }
  def data(pfs : PivotFieldsState) = {

    val filters = pfs.allFilterPaths
    val allFilterPathsUpToFirstReportFilter = filters.chopUpToFirstNon(tradeFields)
    val tradeFilterFields = allFilterPathsUpToFirstReportFilter.allFields.toSet
    val tradeFilterFieldsInTheFilterArea = pfs.filterAreaFields.toSet & tradeFilterFields
    val groupByFields = (pfs.filterAreaFields ++ pfs.rowFields ++ pfs.columns.columnFields).filter(tradeFields.contains).filterNot(tradeFilterFieldsInTheFilterArea.contains)

    val groupedCombinedFieldBindingsBuilder = new GroupedAndCombinedFieldBindingsBuilder(
      tradeFilters = allFilterPathsUpToFirstReportFilter,
      tradeFieldsToGroupBy = groupByFields,
      tradeFieldsToCombine = pfs.columns.measureFields.filter(f => tradeFields.contains(f)),
      pfs.reportSpecificChoices.toMap
    )

    val possibleFilterFieldValuesBuilder = new PossibleValuesBuilder(fieldDetails, allFilterPathsUpToFirstReportFilter)

    for (row <- tradeResult.data) {
      possibleFilterFieldValuesBuilder+=(row)
      groupedCombinedFieldBindingsBuilder+=(row)
    }

    val tradePossibleValues = possibleFilterFieldValuesBuilder.build
    val data = groupedCombinedFieldBindingsBuilder.collectMergedReportValues

    val result = UnfilteredPivotTableDataSource.applyFiltersAndCalculatePossibleValues(
      fieldDetails, data, pfs.removeAll(tradeFilterFields)
    )
    //tradePossibleValues  has the possible values for the trade based fields
    //result.possibleValues has the possible values for the trade based fields and the report based fields
    //but because 'data' is only a subset of all the rows the possible values for trade fields are incorrect
    //For this reason it is important that tradePossibleValues  overwrites result.possibleValues   
    PivotResult(result.data, result.possibleValues ++ tradePossibleValues )
  }

  private def mergedReportOptions = reports.map(_.report.reportSpecificOptions).reduceLeft(_++_).distinct.filter(PivotReport.validReportSpecificChoices(desk))
  override def availablePages = reports.flatMap(_.availablePages).toSet.toList

  override def reportSpecificOptions: List[(String, List[Any])] = mergedReportOptions.distinct.stringValues

  override def zeroFields = reports.flatMap(_.report.zeroFields).toSet

  override def initialState = {
    val initialReportSpecificChoices = mergedReportOptions.default
    val pfs = PivotFieldsState(rowFields=List(Field("Risk Market"), Field("Risk Period")),
                               dataFields=List(Field("Position")),
                               reportSpecificChoices = TreeMap(initialReportSpecificChoices.toArray:_*))

    DefaultPivotState(pfs)
  }
}

