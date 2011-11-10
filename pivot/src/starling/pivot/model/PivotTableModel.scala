package starling.pivot.model

import starling.pivot.controller._
import starling.pivot.FieldChooserType._
import starling.rmi.PivotData
import java.io.Serializable
import java.lang.String
import starling.utils.ImplicitConversions._
import starling.utils.Log
import starling.quantity.Quantity
import starling.pivot.EditableCellState._
import starling.pivot._
import controller.ChildKey
import collection.immutable.{Map, TreeMap}
import collection.mutable.{HashSet, HashMap}

class FieldList(pivotTableModel:PivotTableModel, _fields:Seq[Field], fieldChooserType:FieldChooserType) {
  def get(index:Int) = _fields(index)

  def contains(field:Field) = {_fields.contains(field)}

  def position(field:Field) = {_fields.indexOf(field)}

  def isEmpty = _fields.isEmpty

  def fields:List[Field] = _fields.toList

  def size = _fields.size
}
case class FieldListGroup(name:String, fieldList:FieldList)

case class AxisValueList[T](list:List[T])

class ViewState
object TableViewState extends ViewState
object ChartViewState extends ViewState

abstract class AxisValueType extends Serializable {
  def value:Any //The value in the table
  def cellType:EditableCellState = Normal
  def pivotEdits:PivotEdits = PivotEdits.Null
  def originalValue:Option[Any] = None
  def valueForChildKey(newRowsAtBottom:Boolean) = value
  def originalOrValue = originalValue.getOrElse(value)
}

case class CellTypeSpecifiedAxisValueType(override val cellType:EditableCellState, value:Any, override val pivotEdits:PivotEdits) extends AxisValueType {
  override def originalValue = None
}

case object BlankAddedAxisValueType extends AxisValueType {
  def value = UndefinedValueNew
  override def cellType:EditableCellState = AddedBlank
}

case class ValueAxisValueType(valueX:Any) extends AxisValueType {
  val pivotValue = PivotValue.create(valueX)
  override def cellType = pivotValue.cellType
  override def pivotEdits = pivotValue.edits
  override def originalValue = pivotValue.originalValue
  override def value = pivotValue.axisValueValue
  override def valueForChildKey(newRowsAtBottom:Boolean) = pivotValue.valueForGrouping(newRowsAtBottom)
}

case class NewRowValue(rowIndex:Int)

case object TotalValue {
  override def toString = "Total"
}
case object TotalAxisValueType extends AxisValueType {
  def value = TotalValue
}
case object NullAxisValueType extends AxisValueType {
  def value = ""
}
case class MeasureAxisValueType(field:Field) extends AxisValueType {
  def value = field.name
}

case class AxisValue(field:Field, value:AxisValueType, position:Int, newRowsAtBottom:Boolean=false) {
  def isNull = field.name == "Null"
  def valueText = value.value.toString
  def toTotal = copy(value = TotalAxisValueType)
  def isTotal = value == TotalAxisValueType
  def isOtherValue = (value.value == FilterWithOtherTransform.OtherValue) || (value.value match {
    case p:PivotTreePath if p.isOther => true
    case _ => false
  })
  def isMeasure = value.isInstanceOf[MeasureAxisValueType]
  def state = value.cellType
  def pivotEdits = value.pivotEdits
  def childKey = ChildKey(value.valueForChildKey(newRowsAtBottom), isMeasure, field, position)
}

case object UndefinedValue {
  override def toString = "n/a"
}

case object UndefinedValueNew {
  override def toString = ""
}

case object NoValue {}

object AxisValue {
  val Null = AxisValue(Field.NullField, NullAxisValueType, 0)
}

/**
 * This represents the underlying pivot table model
 * and contains the representation rendered by the views
 */
class PivotTableModel(data:PivotData) {
  private val fieldState = data.pivotFieldsState
  private var firePivotChanged:(PivotFieldsState => Unit) = (fs) => {}
  private var fireWorking = () => {}

  val chooserFields = new FieldList(this, data.allFields.toList.filterNot(fieldState.allFieldsUsed.contains), FieldList)
  val rowFields = new FieldList(this, fieldState.rowFields, Rows)
  val columnFields = new FieldList(this, List(), Columns)
  val columns = fieldState.columns

  val filterFields = new FieldList(this, fieldState.filters.map(_._1).filterNot(f => rowFields.contains(f) || columns.contains(f)), Filter)

  def isMeasureField(field:Field) = data.dataFields.contains(field)

  def getFields(fieldChooserType:FieldChooserType) = {
    fieldChooserType match {
      case Filter => filterFields
      case Rows => rowFields
      case Columns => columnFields
      case FieldList => chooserFields
    }
  }

  def getGroupedFields(fieldChooserType:FieldChooserType):List[FieldListGroup] = {
    fieldChooserType match {
      case FieldList => {
        data.fieldGroups.map(fieldGroup => FieldListGroup(fieldGroup.name, new FieldList(this, chooserFields.fields.intersect(fieldGroup.fields), FieldList)))
      }
      case _ => List()
    }
  }

  def treeDetails = data.pivotTable.treeDetails

  private val _filters = fieldState.filters

  def publishFieldStateChange(field:Field, position:Int, from:FieldChooserType, to:FieldChooserType) = {
    val currentState = getCurrentPivotFieldsState
    val updatedState = currentState.moveField(field, from, to, position)
    // Return true if there has been no effect.
    if (updatedState != currentState) {
      publishFieldStateUpdated(updatedState)
      false
    } else {
      true
    }
  }

  def publishFieldStateChange(field:Field, newColumnStructure:ColumnTrees, from:FieldChooserType) = {
    val currentState = getCurrentPivotFieldsState
    val updatedState = currentState.moveField(field, from, newColumnStructure)
    // Return true if there has been no effect.
    if (updatedState != currentState) {
      publishFieldStateUpdated(updatedState)
      false
    } else {
      true
    }
  }

  def publishFieldStateUpdated(fieldState:PivotFieldsState) {
    firePivotChanged(fieldState)
  }

  def filtersWithSome = {
    _filters.filter(f => f._2 match {case SomeSelection(_) => true; case _ => false})
  }

  def getCurrentPivotFieldsState = {
    fieldState//new PivotFieldsState(rowFields.fields, columns, _filters, totals, treeDetails.treeDepths, fieldState.reportSpecificChoices, fieldState.transforms)
  }

  def setPivotChangedListener(listener:(PivotFieldsState => Unit)) {
    firePivotChanged = listener
  }

  def setWorkingListener(listener:() => Unit) {
    fireWorking = listener
  }

  def setFilter(field:Field, selection:Selection) {
    val localFilters = _filters.map(t => (t._1,
            if (t._1 == field) {
              selection
            } else {
              t._2
            })) ::: (if (!_filters.exists(_._1 == field)) List((field, selection)) else Nil)
    val changed = localFilters != _filters
    if (changed) {
      firePivotChanged(getCurrentPivotFieldsState.copy(filters = localFilters.toList, transforms = scala.collection.immutable.TreeMap.empty))
    }
  }

  def setTransform(field:Field, transform:FilterWithOtherTransform) {
    val newTransforms = fieldState.transforms + (field -> transform)
    val newFilters = fieldState.filters.filterNot{case (f,selection) => f == field}
    if ((newTransforms != fieldState.transforms) || (newFilters != fieldState.filters)) {
      firePivotChanged(getCurrentPivotFieldsState.copy(filters = newFilters, transforms = newTransforms))
    }
  }

  def setDepth(field:Field, depth:(Int, Int)) {
    val newDepths = getCurrentPivotFieldsState.treeDepths + (field -> depth)
    firePivotChanged(getCurrentPivotFieldsState.copy(treeDepths = newDepths))
  }

  def getFilter(field:Field):Option[Selection] = {
    _filters.find(t => t._1 == field) match {
      case Some((f, selection)) => Some(selection)
      case None => None
    }
  }

  def getRowFields = rowFields.fields
  //def getColFields = columnFields.fields

  def possibleValuesAndSelection(field:Field):Option[(TreePivotFilter, Selection)] = {
    if (data.pivotTable == null) return None

    data.pivotTable.possibleValues.get(field) match {
      case Some(filter) => {
        getFilter(field) match {
          case Some(AllSelection) => Some((filter, AllSelection))
          case Some(SomeSelection(selection)) => Some((filter, SomeSelection(selection))) //Some((filter, filter & selection))
          case None => Some((filter, AllSelection))
        }
      }
      case None => None
    }
  }

  def transforms(field:Field) = fieldState.transforms.get(field)
}


trait DataFieldTotal {
  def addValue(value:PivotValue):DataFieldTotal
  def addGroup(other:DataFieldTotal):DataFieldTotal
  def isAlmostZero:Boolean
  def measureCell:MeasureCell
  def aggregateValue:Option[Any] = None
  def aggregateOriginal:Option[Any] = None
  def edits:PivotEdits = PivotEdits.Null
}

object NullDataFieldTotal extends DataFieldTotal {
  def addValue(value:PivotValue) = throw new Exception()
  def addGroup(other:DataFieldTotal) = other
  def isAlmostZero = true
  def measureCell = MeasureCell.Null
}

case class EmptyDataFieldTotal(fieldDetails:FieldDetails, editable:Boolean, blankCellsEditable:Boolean) extends DataFieldTotal {
  def addValue(value:PivotValue) = SingleDataFieldTotal(fieldDetails, value, editable)
  def addGroup(other:DataFieldTotal) = other
  def isAlmostZero = false // TODO - look at this
  def measureCell = {
    if (blankCellsEditable) {
      MeasureCell.EditableNull
    } else {
      MeasureCell.Null
    }
  }
}

trait NonEmptyDataFieldTotal extends DataFieldTotal {
  def fieldDetails:FieldDetails
  def addValue(v:PivotValue) = {
    val combinedValues = fieldDetails.combineValueOption(aggregateValue, v.value)
    val combinedOriginal = fieldDetails.combineValueOption(aggregateOriginal, v.originalValue)
    CombinedDataFieldTotal(fieldDetails, combinedValues, combinedOriginal, edits.addEdits(v.edits))
  }
  def addGroup(other:DataFieldTotal) = {
    val combinedValues = fieldDetails.combineOptions(aggregateValue, other.aggregateValue)
    val combinedOriginal = fieldDetails.combineOptions(aggregateOriginal, other.aggregateOriginal)
    CombinedDataFieldTotal(fieldDetails, combinedValues, combinedOriginal, edits.addEdits(other.edits))
  }
  def isAlmostZero = aggregateOriginal.isEmpty && (aggregateValue match {
    case None => true
    case Some(q:Quantity) => q.isAlmostZero
    case Some(pq:PivotQuantity) => pq.isAlmostZero
    case _ => false
  })
}

case class SingleDataFieldTotal(fieldDetails:FieldDetails, value:PivotValue, editable:Boolean) extends NonEmptyDataFieldTotal {
  override def aggregateValue = value.value.map(v=>fieldDetails.combineFirstGroup(v))
  override def aggregateOriginal = value.originalValue.map(v=>fieldDetails.combineFirstGroup(v))
  def measureCell = {
    val vv = value.valueOrOriginal match {
      case Some(valueOrDeletedValue) => Some(fieldDetails.value(fieldDetails.combineFirstGroup(valueOrDeletedValue)))
      case None => None
    }
    MeasureCell(vv, value.cellType, edits, value.originalValue, editable)
  }
  override val edits = value.edits
}

case class CombinedDataFieldTotal(fieldDetails:FieldDetails, override val aggregateValue:Option[Any],
                                  override val aggregateOriginal:Option[Any],
                                  override val edits:PivotEdits) extends NonEmptyDataFieldTotal {
  def measureCell = {
    val valueOrDeletedValue = if (aggregateValue.isDefined) aggregateValue else aggregateOriginal
    val vv = valueOrDeletedValue.map(v => fieldDetails.value(v))
    val cellType = if (edits.nonEmpty) Tainted else Normal
    MeasureCell(vv, cellType, edits)
  }
}

object PivotTableModel {
  def setDefaultReportSpecificChoices(reportSpecificOptions:List[(String,List[Any])], fs:PivotFieldsState) = {
    if (fs.reportSpecificChoices.isEmpty && reportSpecificOptions.nonEmpty) {
      // I need to set the default report specific choices so that layouts are consistent.
      val defaultReportSpecificChoices = Map() ++ reportSpecificOptions.map{case (label,choices) => {
        choices match {
          case List(first:Boolean, second:Boolean) if first != second => (label -> first)
          case _ => (label -> choices(0))
        }
      }}
      fs.copy(reportSpecificChoices = TreeMap(defaultReportSpecificChoices.toArray:_*))
    } else {
      fs
    }
  }

  def createPivotData(dataSource:PivotTableDataSource, pivotFieldParams:PivotFieldParams):PivotData = {
//    dataSource.fieldDetailsGroups.flatMap(_.fields.map(_.field.name)).update { fieldNames =>
//      require(!fieldNames.containsDuplicates, "DataSource: " + dataSource.getClass + " contains duplicate fields: " + fieldNames.sortWith(_ < _))
//    }

    val (fs, pivotTable) = if (pivotFieldParams.calculate) {
      _createPivotTableData(dataSource, pivotFieldParams.pivotFieldState)
    } else {
      val pfs = pivotFieldParams.pivotFieldState.getOrElse(PivotFieldsState.Blank)
      val pt = PivotTable.singleCellPivotTable("Calculation is off")
      (pfs, pt)
    }

    val reportSpecificOptions = dataSource.reportSpecificOptions
    val fsToUse = setDefaultReportSpecificChoices(reportSpecificOptions, fs)

    val initialState = dataSource.initialState
    PivotData(
      allFields = dataSource.fieldDetails.map(_.field),
      fieldGroups = dataSource.fieldDetailsGroups.map(_.toFieldGroup),
      dataFields = dataSource.fieldDetails.filter(_.isDataField).map(_.field).toSet,
      pivotFieldsState = fsToUse,
      drillDownGroups = dataSource.drillDownGroups,
      pivotTable = pivotTable,
      availablePages = dataSource.availablePages,
      if (initialState == PivotFieldsState.Blank) None else Some(initialState),
      reportSpecificOptions = reportSpecificOptions)
  }

  def createPivotTableData(dataSource:PivotTableDataSource, pivotState:Option[PivotFieldsState]):PivotTable = {
    _createPivotTableData(dataSource, pivotState)._2
  }

  def _createPivotTableData(pivot:PivotTableDataSource, pivotState:Option[PivotFieldsState]) = {
    val fs = pivotState match {
      case Some(f) => {
        val fields = Set() ++ pivot.fieldDetails.map(_.field)
        if (f.allFieldsUsed.forall(fields.contains(_))) {
          f
        } else {
          pivot.initialState
        }
      }
      case None => pivot.initialState
    }
    (fs, createPivotTableData(pivot, fs))
  }

  private def allEditableInfo(fieldDetailsLookup:Map[Field,FieldDetails], editPivot0:Option[EditPivot],
                              pivotState:PivotFieldsState):AllEditableInfo = {
    editPivot0 match {
      case None => {
        val em:Set[Field] = Set.empty
        AllEditableInfo(None, em, false)
      }
      case Some(editPivot) => {
        val keyFields = editPivot.editableToKeyFields.flatMap(_._2).toSet
        val measureFieldsToParser = editPivot.editableToKeyFields.keySet.map(f => f -> fieldDetailsLookup(f).parser).toMap
        val keyFieldsToParser = editPivot.editableToKeyFields.values.toSet.flatten.map(f => f -> fieldDetailsLookup(f).parser).toMap

        val extraLine = editPivot.editableToKeyFields.exists{case (editableField,kFields) => {
          pivotState.columns.contains(editableField) && keyFieldsInColumnArea(kFields.toSet, pivotState).isEmpty
        }}

        val allEditableMeasures = editPivot.editableToKeyFields.keySet

        val blankEditable = keyFields.forall(f => {
          pivotState.rowFields.contains(f) ||
                  pivotState.columns.contains(f) ||
                  pivotState.filtersInTheFilterArea.exists{case (f0, sel) => {
                    (f == f0) && (sel match {
                      case SomeSelection(s) if s.size == 1 => true
                      case _ => false
                    })
                  }}
        })

        // I have to pull out the set here as there is a strange serializable problem if I don't.
        val measureFieldsSet = Set() ++ measureFieldsToParser.keySet

        AllEditableInfo(Some(EditableInfo(keyFields, measureFieldsSet, measureFieldsToParser ++ keyFieldsToParser, extraLine, blankEditable)), allEditableMeasures, blankEditable)
      }
    }
  }

  private def generateMainData(pivotState:PivotFieldsState, pivotResult:PivotResult,
                               fieldDetailsLookup:Map[Field,FieldDetails], treeDepths:Map[Field,(Int, Int)],
                               allEditableInfo:AllEditableInfo):(HashMap[(List[ChildKey],List[ChildKey]), DataFieldTotal], ServerAxisNode, ServerAxisNode, Map[Field,Int]) = {
    //Make one pass through all the rows building up row and column trees
    //and the 'sum' of the data area values for each row-column pair

    val filters = pivotState.filters.toMap
    val measureFields = pivotState.columns.measureFields.toSet
    val fieldsToDepth = treeDepths.map{case (field, (start, end)) => {
      if (measureFields.contains(field)) {
        field -> 1
      } else {
        field -> (end - start + 1)
      }
    }}
    val allPaths = pivotState.columns.buildPathsWithPadding(fieldsToDepth)
    val maxColumnDepth = if (allPaths.isEmpty) 0 else allPaths.maximum(_.path.map{ case (field,_) => fieldsToDepth.getOrElse(field, 1)}.sum)

    val mainTableBucket = new scala.collection.mutable.HashMap[(List[ChildKey], List[ChildKey]), DataFieldTotal]
    var rowAxisRoot = ServerAxisNode.Null
    var columnAxisRoot = ServerAxisNode.Null
    val maxDepths = new scala.collection.mutable.HashMap[Field, Int]

    val forceNewRowsToBottom = allEditableInfo.editableInfo.map(_.extraLine).getOrElse(false)

    val rowValuesAlreadyAdded = new HashSet[AxisValueList[AxisValue]]()
    val columnValuesAlreadyAdded = new HashSet[AxisValueList[AxisValue]]()

    for (row <- pivotResult.data) {
      def valuesForField(field:Field) : List[Any] = {
        val rawValue = row.getOrElse(field, UndefinedValue)
        val fd = fieldDetailsLookup(field)
        fd match {
          case _:TreeFieldDetails => {
            val path = rawValue.asInstanceOf[PivotTreePath]
            val (start, end) = treeDepths(field)
            pivotState.transforms.get(field) match {
              case Some(FilterWithOtherTransform(selection)) => {
                val selectedPaths = selection.asInstanceOf[Set[PivotTreePath]]
                if (selectedPaths.exists(_.equalOrParentOf(path))) {
                  maxDepths.getOrElseUpdate(field, {scala.math.max(maxDepths.getOrElse(field, 0), path.size)})
                  path.between(start, end).reverse
                } else {
                  (start to end).map(c => {
                    val paths = List.fill(c)(FilterWithOtherTransform.Other.toString)
                    PivotTreePath(paths)
                  }).toList.reverse
                }
              }
              case None => {
                maxDepths.getOrElseUpdate(field, {scala.math.max(maxDepths.getOrElse(field, 0), path.size)})
                path.between(start, end).reverse
              }
            }
          }
          case _ => {
            val value = {
              val v = fd.transformValueForGroupByField(rawValue)
              pivotState.transforms.get(field) match {
                case Some(FilterWithOtherTransform(selection)) => {
                  if (selection.contains(v)) v else FilterWithOtherTransform.Other
                }
                case None => v
              }
            }
            value :: Nil
          }
        }
      }

      val rowValuesOption:Option[AxisValueList[AxisValue]] = {
        val dataFields = pivotState.columns.measureFields

        def buildRow(soFar:List[AxisValue], left:List[Field]):Option[List[AxisValue]] = {
          left match {
            case Nil => Some(soFar)
            case field :: tail => {
              val values = valuesForField(field)
              val axisValues = values.map{ v => AxisValue(field, new ValueAxisValueType(v), 0, forceNewRowsToBottom) }
              buildRow(axisValues ::: soFar, tail)
            }
          }
        }
        if (dataFields.isEmpty || dataFields.exists(row.contains(_))) {
          buildRow(List(), pivotState.rowFields).map(_.reverse)
        } else {
          //If dataFields are specified only show rows which have a value for one of the fields
          None
        }
      }.map(l=>new AxisValueList(l))

      val columnValuesList:List[(Option[Field], AxisValueList[AxisValue])] = {
        allPaths.flatMap {
          path => {
            def getAxisValues(head:Field, index:Int) = {
              val someValue = PivotValue.extractValue(row, head)
              val matches = filters.get(head).map{ selection=>selection.matches(fieldDetailsLookup(head), someValue) }.getOrElse(true)
              if (!matches) {
                None
              } else {
                val values = valuesForField(head)
                Some(values.map{ v => AxisValue(head, new ValueAxisValueType(v), index) })
              }
            }

            path.dataField match {
              case Some(df) => row.get(df) match { //This path has a datafield
                case Some(dataValue) => { //This row has a value for the datafield

                  def buildKey(soFar:List[AxisValue], fields:List[(Field, Int)]):Option[(Option[Field], List[AxisValue])] = {
                    fields match {
                      case Nil => Some((Some(df), soFar))
                      case (head, index) :: rest if (head == df) => buildKey(AxisValue(head, MeasureAxisValueType(df), index) :: soFar, rest)
                      case (head, index) :: rest => {
                        getAxisValues(head, index) match {
                          case None => None
                          case Some(axisValues) => buildKey(axisValues ::: soFar, rest)
                        }
                      }
                    }
                  }
                  buildKey(List(), path.path)
                }
                case None => None
              }
              case None => {
                def buildKey(soFar:List[AxisValue], fields:List[(Field, Int)]):Option[(Option[Field], List[AxisValue])] = {
                  fields match {
                    case Nil => Some((None, soFar))
                    case (head, index) :: rest => {
                      getAxisValues(head, index) match {
                        case None => None
                        case Some(axisValues) => buildKey(axisValues ::: soFar, rest)
                      }
                    }
                  }
                }
                buildKey(List(), path.path)
              }
            }
          }
        }.map {
          case (df, values) =>
            val path = values.reverse
            val padding = (for (i <- path.size until maxColumnDepth) yield AxisValue.Null).toList
            (df, new AxisValueList(path ::: padding))
        }
      }

      if (columnValuesList.nonEmpty) {
        for ((dataFieldOption, columnValues) <- columnValuesList) {
          for (rowValues <- rowValuesOption) {

            if (!rowValuesAlreadyAdded.contains(rowValues)) {
              rowAxisRoot = rowAxisRoot.add(rowValues.list)
              rowValuesAlreadyAdded += rowValues
            }
            if (!columnValuesAlreadyAdded.contains(columnValues)) {
              columnAxisRoot = columnAxisRoot.add(columnValues.list)
              columnValuesAlreadyAdded += columnValues
            }

            val rowColumnKey = (rowValues.list.map(_.childKey), columnValues.list.map(_.childKey))
            dataFieldOption.foreach { df => {
              if (!mainTableBucket.contains(rowColumnKey)) {
                mainTableBucket(rowColumnKey) = new EmptyDataFieldTotal(fieldDetailsLookup(df), allEditableInfo.editableMeasures.contains(df), allEditableInfo.blankCellsEditable)
              }
              mainTableBucket(rowColumnKey) = mainTableBucket(rowColumnKey).addValue(PivotValue.create(row(df)))
            }}
          }
        }
      } else if (rowValuesOption.isDefined && rowValuesOption.get.list.nonEmpty) {
        for (rowValues <- rowValuesOption if !rowValuesAlreadyAdded.contains(rowValues)) {
          rowAxisRoot = rowAxisRoot.add(rowValues.list)
          rowValuesAlreadyAdded += rowValues
        }
      }
    }

    val rowTotals = rowAxisRoot.withTotals
    val columnTotals = columnAxisRoot.withTotals
    (mainTableBucket, rowTotals, columnTotals, maxDepths.toMap)
  }

  private def withSubtotals(mainTableBucket:HashMap[(scala.List[ChildKey],scala.List[ChildKey]), DataFieldTotal]) = {
    def permutations(list:List[ChildKey]):List[List[ChildKey]] = {
      val values = (list.size to 0 by -1).map(col => {
        val (start,end) = list.splitAt(col)
        start ::: end.map(c => if (c.isMeasure) c else c.toTotal)
      }).toList
      val distinctValues = values.distinct
      distinctValues
    }

    mainTableBucket.toList.foreach {
      case ((row, column), sum) => {
        val columnPermutations = permutations(column)
        val rowPermutations = permutations(row)
        for (rowP <- rowPermutations;
             colP <- columnPermutations) {
          val key = (rowP, colP)
          if (key != (row, column)) {
            val current = mainTableBucket.getOrElseUpdate(key, NullDataFieldTotal)
            mainTableBucket(key) = current.addGroup(sum)
          }
        }
      }
    }
    mainTableBucket
  }

  private def compress(bucket:HashMap[(scala.List[ChildKey],scala.List[ChildKey]), DataFieldTotal]) = {
    val equalToReferenceTableCellMap = new HashMap[Any,MeasureCell]
    val equalToReferenceAxisValueListMap = new HashMap[List[ChildKey],List[ChildKey]]
    val equalToReferenceAxisValue = new HashMap[ChildKey,ChildKey]

    def compress(k1:List[ChildKey]) = {
      val compressed = k1.map( v => equalToReferenceAxisValue.getOrElseUpdate(v,v))
      equalToReferenceAxisValueListMap.getOrElseUpdate(compressed,compressed)
    }

    val compressedBucket = bucket.map{case ((k1,k2),v) => {
      val cv = v.measureCell
      ((compress(k1),compress(k2)),
              equalToReferenceTableCellMap.getOrElseUpdate(cv,cv))}
    }
    compressedBucket.toMap
  }

  private def possibleValuesToTree(pivotResult:PivotResult, fieldDetailsLookup:Map[Field,FieldDetails]) = {
    pivotResult.possibleValues.map {
      case (field, unsortedValues) => {
        val fd = fieldDetailsLookup(field)
        fd match {
          case _:TreeFieldDetails => {
            val pivotTreePaths = unsortedValues.filterNot(_ == UndefinedValue).castValues[PivotTreePath] { uncastable =>
              throw new IllegalStateException("The value " + uncastable + " for " + field + " is not a PivotPathTree") }

            field -> toTree(pivotTreePaths, fd.comparator)
          }
          case _ => {
            val values = {
              // We are using java sorting here because everything is java comparable where as strings are not scala ordered.
              val unsortedPivotValuesOrUndefined = unsortedValues.map { value => {
                val v1 = PivotValue.create(value)
                v1.originalValue.getOrElse(v1.value.getOrElse(UndefinedValue))
              } }
              val (nullValues, normalValues) = unsortedPivotValuesOrUndefined.partition(v => {v == UndefinedValue || v == UndefinedValueNew})
              val arrayOfObjects = normalValues.toArray.asInstanceOf[Array[Object]]
              try {
                val sorted = arrayOfObjects.sorted(new Ordering[Any]() {
                  def compare(x:Any, y:Any) = {
                    fd.comparator.compare(x, y)
                  }
                })
                nullValues ::: sorted.toList
              } catch {
                case e =>
                  println("Invalid objects, could not sort: " + fd.field)
                  arrayOfObjects.foreach{x => println(x + ", " + x.getClass)}
                  throw e
              }
            }
            field -> TreePivotFilter(TreePivotFilterNode(AllFilterSelection, values.map(v => TreePivotFilterNode(v, Nil))))
          }
        }
      }
    }
  }

  private def fieldIndexes(fields:List[Field], treeDepths:Map[Field,(Int,Int)], fieldDetailsLookup:Map[Field,FieldDetails]):Array[Int] = {
    fields.map {
      field => {
        val fd = fieldDetailsLookup(field)
        fd match {
          case _:TreeFieldDetails => (treeDepths(field)._2 - treeDepths(field)._1) + 1
          case _=> 1
        }
      }
    }.toArray
  }

  def createPivotTableData(dataSource:PivotTableDataSource, pivotState:PivotFieldsState):PivotTable = {
    if (pivotState.columns.isInvalid) {
      PivotTable.singleCellPivotTable("Too many measures")
    } else {
      val then = System.currentTimeMillis

      val fieldDetailsLookup = Map() ++ dataSource.fieldDetails.map(fd => fd.field -> fd) + (Field.NullField -> FieldDetails("Null"))
      val allEditableInfo0 = allEditableInfo(fieldDetailsLookup, dataSource.editable, pivotState)
      val treeDepths = Map() ++ dataSource.fieldDetails.filter(_.isInstanceOf[TreeFieldDetails]).map(_.field).map(field => field -> pivotState.treeDepths.getOrElse(field, (3, 7)))

      val pivotResult = Log.debugWithTimeGapTop("Data from data source"){dataSource.data(pivotState)}
      val (mainTableBucket, rowAxisRoot, columnAxisRoot, maxDepths) = Log.infoWithTime("Generating bucket"){generateMainData(pivotState, pivotResult, fieldDetailsLookup, treeDepths, allEditableInfo0)}
      val mainTableBucketWithSubtotals = Log.debugWithTimeGapBottom("Adding subtotals"){withSubtotals(mainTableBucket)}
      val compressedMap = compress(mainTableBucketWithSubtotals)
      val possibleValuesConvertedToTree = possibleValuesToTree(pivotResult, fieldDetailsLookup)
      val rowFieldHeadingCount = fieldIndexes(pivotState.rowFields, treeDepths, fieldDetailsLookup)
      val fieldInfo = FieldInfo(Map() ++ fieldDetailsLookup.map{case (f,fd) => (f -> fd.formatter)}, treeDepths.keySet.toList)

      // determine how long it runs for
      val now = System.currentTimeMillis
      Log.debug("Pivot Table Model generated in " + (now - then) + "ms")
      PivotTable(pivotState.rowFields, rowFieldHeadingCount, rowAxisRoot.toGUIAxisNode(fieldDetailsLookup),
        columnAxisRoot.toGUIAxisNode(fieldDetailsLookup), possibleValuesConvertedToTree,
        TreeDetails(treeDepths, maxDepths), allEditableInfo0.editableInfo, fieldInfo, compressedMap, dataSource.zeroFields.toSet)
    }
  }

  private def keyFieldsInColumnArea(keyFields:Set[Field], pfs:PivotFieldsState): Set[Field] = {
    val filterAreaFieldsMap = Map() ++ pfs.filtersInTheFilterArea
    val keyFieldsNotInRowArea = keyFields.filterNot(pfs.rowFields.contains)
    keyFieldsNotInRowArea.filterNot(f => {
      filterAreaFieldsMap.contains(f) && (filterAreaFieldsMap(f) match {
        case SomeSelection(v) if v.size == 1 => true
        case _ => false
      })
    })
  }

  def toTree(paths: scala.List[PivotTreePath], comparator: scala.Ordering[Any]): TreePivotFilter = {
    val sortedPaths = paths.sorted(comparator)

    Log.debug("toTree.sortedPaths (with comparator: %s):\n" % (comparator, sortedPaths.mkString("\n\t")))

    val merged = TreePivotFilterNode.mergeForests(sortedPaths.map(_.toTree))

    merged.size match {
      case 0 => TreePivotFilter(TreePivotFilterNode("None", Nil))
      case 1 => TreePivotFilter(merged.head.copy(children = merged))
      case _ => throw new Exception("Expect all paths to have a common root node: " + sortedPaths.map(_.path.head))
    }
  }
}

case class TreeDetails(treeDepths:Map[Field, (Int, Int)], maxTreeDepths:Map[Field, Int])

case class EditableInfo(keyFields:Set[Field], measureFields:Set[Field], fieldToParser:Map[Field,PivotParser], extraLine:Boolean, blankCellsEditable:Boolean)
case class FieldInfo(fieldToFormatter:Map[Field,PivotFormatter], treeFields:List[Field])
object FieldInfo {
  val Blank = FieldInfo(Map(), Nil)
}

case class AllEditableInfo(editableInfo:Option[EditableInfo], editableMeasures:Set[Field], blankCellsEditable:Boolean)
