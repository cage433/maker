package starling.pivot

import collection.Seq
import collection.mutable.LinkedHashSet
import model.UndefinedValue
import starling.utils.NamedThreadFactory
import java.util.concurrent.{Callable, Executors, Executor}

/**
 * Two pivot tables joined on above the other with the union of the fields
 *
 * Logically it should behave as if the value for all fields which are not present
 * in the other pivot is the null value for the field (FieldDetails#nullValue) 
 */
object UnionPivotTableDataSource {
  def join(pivots:List[PivotTableDataSource]):PivotTableDataSource = {
    pivots.reduceLeft(new UnionPivotTableDataSource(_, _))
  }

  def unionFieldDetails(a:List[FieldDetailsGroup], b:List[FieldDetailsGroup]) = {
    val groupsUpdated = a.map(groupA => {
      b.find(_.name == groupA.name) match {
        case Some(groupAInB) => {
          val groupAFields = groupA.fields.map(_.field).toSet
          groupA.copy(fields = groupA.fields ::: groupAInB.fields.filterNot(fd => groupAFields.contains(fd.field)))
        }
        case None => groupA
      }
    })
    val extraGroups = b.filterNot(groupB => a.map(groupA => groupA.name).contains(groupB.name))
    groupsUpdated ::: extraGroups
  }
}

class UnionPivotTableDataSource(a:PivotTableDataSource, b:PivotTableDataSource) extends PivotTableDataSource {
  val fieldDetailsGroups = UnionPivotTableDataSource.unionFieldDetails(a.fieldDetailsGroups, b.fieldDetailsGroups)

  val fieldDetailsMap = Map() ++ fieldDetails.map { f => f.field -> f }
  private def pivotResult(pfs:PivotFieldsState, pivotTableDataSource:PivotTableDataSource):PivotResult = {
    pivotTableDataSource.data(pfs)
  }
  def data(pfs : PivotFieldsState) = {
    val executorService = Executors.newFixedThreadPool(2, NamedThreadFactory("union worker"))
    val futureA = executorService.submit(new Callable[PivotResult]{def call = pivotResult(pfs, a)})
    val futureB = executorService.submit(new Callable[PivotResult]{def call = pivotResult(pfs, b)})
    val resultA = futureA.get
    val resultB = futureB.get
    executorService.shutdown()

    val dataUnion = resultA.data ++ resultB.data

    val possibleValuesUnion = Map() ++ (resultA.possibleValues.keysIterator ++ resultB.possibleValues.keysIterator).map{
      field => {
        val set = new LinkedHashSet[Any]()
        set ++= resultA.possibleValues.getOrElse(field, List(UndefinedValue))
        set ++= resultB.possibleValues.getOrElse(field, List(UndefinedValue))
        field -> set.toList
      }}
    PivotResult(dataUnion, possibleValuesUnion)
  }

  //for now just use the drilldown behaviour of the first pivot
  override def initialState = {
    val newPFS = a.initialState.pfs.copy(reportSpecificChoices = a.initialState.pfs.reportSpecificChoices ++ b.initialState.pfs.reportSpecificChoices)
    a.initialState.copy(pfs = newPFS)
  }
  override def drillDownGroups = if (a.drillDownGroups.nonEmpty) a.drillDownGroups else b.drillDownGroups
  override def availablePages = a.availablePages

  override def reportSpecificOptions = (a.reportSpecificOptions ++ b.reportSpecificOptions).distinct
  override def zeroFields = a.zeroFields ++ b.zeroFields
}