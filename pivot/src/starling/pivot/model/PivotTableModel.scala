package starling.pivot.model

import starling.pivot._

import controller._
import starling.pivot.FieldChooserType._
import starling.rmi.PivotData
import java.io.Serializable
import collection.mutable.HashMap
import collection.immutable.{List, TreeMap}
import java.lang.String
import starling.utils.ImplicitConversions._
import starling.utils.Log
import starling.quantity.{SpreadOrQuantity, Quantity}


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
  def value:Any
}
case class ValueAxisValueType(value:Any) extends AxisValueType

case object TotalAxisValueType extends AxisValueType {
  def value = "Total"
}
case object NullAxisValueType extends AxisValueType {
  def value = ""
}
case class MeasureAxisValueType(field:Field) extends AxisValueType {
  def value = field.name
}

case class AxisValue(field:Field, value:AxisValueType, position:Int) {
  def valueText = value.value.toString
  def toTotal = copy(value = TotalAxisValueType)
  def isTotal = value == TotalAxisValueType
  def isOtherValue = value.value == FilterWithOtherTransform.OtherValue
  def isMeasure = value.isInstanceOf[MeasureAxisValueType]

  def <(other:AxisValue, comparator:Ordering[Any]):Boolean = {
    if (position == other.position) {
      def index(value:AxisValueType) = {
        value match {
          case TotalAxisValueType => 0
          case ValueAxisValueType(UndefinedValue) => 1
          case ValueAxisValueType(FilterWithOtherTransform.OtherValue) => 3
          case _ => 2
        }
      }
      val thisIndex = index(value)
      val otherIndex = index(other.value)
      if (thisIndex == otherIndex) {
        if (thisIndex == 2) {
          comparator.compare(this.value.value, other.value.value) < 0
        } else {
          false
        }
      } else {
        thisIndex < otherIndex
      }
    } else {
      position < other.position
    }
  }
}

case object UndefinedValue {
  override def toString = "n/a"
}

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
    val updatedState = getCurrentPivotFieldsState.moveField(field, from, to, position)
    publishFieldStateUpdated(updatedState)
  }

  def publishFieldStateChange(field:Field, newColumnStructure:ColumnTrees, from:FieldChooserType) {
    val updatedState = getCurrentPivotFieldsState.moveField(field, from, newColumnStructure)
    publishFieldStateUpdated(updatedState)
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


//represents the totals for a single data field at the end of a path
class DataFieldTotal(var foo:Option[(FieldDetails, Any)]) {
  def this(fieldDetails:FieldDetails) = this (Some((fieldDetails, fieldDetails.nullGroup)))

  override def toString = "DFT:" + foo.map(_._2.toString).getOrElse("")

  def this() = this (None)

  var mixed = false

  def addValue(value:Any) {
    if (!mixed) {
      foo match {
        case None => throw new Exception()
        case Some((fd, gv)) => foo = Some((fd, fd.combine(gv, value)))
      }
    }
  }

  def mutatingCombineGroup(other:DataFieldTotal) {
    if (!mixed) {
      foo match {
        case None => foo = other.foo
        case Some((thisFd, thisGv)) => {
          other.foo match {
            case Some((fd, gv)) => { //What about mixed field details?
              if (thisFd.field != fd.field) {
                mixed = true
              } else {
                foo = Some((fd, fd.combineGroup(thisGv, gv)))
              }
            }
            case None =>
          }
        }
      }
    }
  }

  def cellValue = foo match {case None => TableCell.Null; case Some((fd, gv)) => fd.value(gv)}
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
      val pfs = pivotFieldParams.pivotFieldState.getOrElse(new PivotFieldsState())
      val pt = PivotTable(List(), Array(), List(), List(), Map(), TreeDetails(Map(), Map()), None, FormatInfo.Blank)
      (pfs, pt)
    }

    val reportSpecificOptions = dataSource.reportSpecificOptions
    val fsToUse = setDefaultReportSpecificChoices(reportSpecificOptions, fs)

    PivotData(
      allFields = dataSource.fieldDetails.map(_.field),
      fieldGroups = dataSource.fieldDetailsGroups.map(_.toFieldGroup),
      dataFields = Set() ++ dataSource.fieldDetails.filter(_.isDataField).map(_.field),
      pivotFieldsState = fsToUse,
      drillDownGroups = dataSource.drillDownGroups,
      pivotTable = pivotTable,
      availablePages = dataSource.availablePages,
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

  def createPivotTableData(dataSource:PivotTableDataSource, pivotState:PivotFieldsState):PivotTable = {
    if (pivotState.columns.isInvalid) {
      PivotTable.singleCellPivotTable("Too many measures")
    } else {
      val then = System.currentTimeMillis
      val filters = pivotState.filters.toMap
      val treeDepths = Map() ++ dataSource.fieldDetails.filter(_.isInstanceOf[TreeFieldDetails]).map(_.field).map(field => field -> pivotState.treeDepths.getOrElse(field, (3, 7)))
      val maxDepths = new scala.collection.mutable.HashMap[Field, Int]

      val fieldDetailsLookup = Map() ++ dataSource.fieldDetails.map(fd => fd.field -> fd) + (Field.NullField -> FieldDetails("Null"))

      val pivotResult = dataSource.data(pivotState)

      val rowAxisValues = scala.collection.mutable.HashSet[AxisValueList[AxisValue]]()
      val columnAxisValues = scala.collection.mutable.HashSet[AxisValueList[AxisValue]]()

      val allPaths = pivotState.columns.buildPaths()

      val maxColumnDepth = if (allPaths.isEmpty) 0 else allPaths.maximum(_.path.size)

      //Make one pass through all the rows building up row and column trees
      //and the 'sum' of the data area values for each row-column pair
      val mainTableBucket = new scala.collection.mutable.HashMap[(AxisValueList[AxisValue], AxisValueList[AxisValue]), DataFieldTotal]

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
                  if (selection.contains(path)) {
                    maxDepths.getOrElseUpdate(field, {scala.math.max(maxDepths.getOrElse(field, 0), path.size)})
                    path.between(start, end).reverse
                  } else {
                    FilterWithOtherTransform.treeNode :: (for (i <- start until end) yield "").toList
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
                val axisValues = values.map{ v => AxisValue(field, new ValueAxisValueType(v), 0) }
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
                val someValue = row.getOrElse(head, UndefinedValue)
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
        for (rowValues <- rowValuesOption) {
          rowAxisValues += rowValues
        }
        for ((_, columnValues) <- columnValuesList) {
          columnAxisValues += columnValues
        }
        for ((dataFieldOption, columnValues) <- columnValuesList if dataFieldOption.isDefined) {
          for (rowValues <- rowValuesOption) {
            val rowColumnKey = (rowValues, columnValues)
            if (!mainTableBucket.contains(rowColumnKey)) {
              mainTableBucket(rowColumnKey) = dataFieldOption match {
                case None => new DataFieldTotal()
                case Some(df) => new DataFieldTotal(fieldDetailsLookup(df))
              }
            }
            dataFieldOption match {
              case None =>
              case Some(df) => mainTableBucket(rowColumnKey).addValue(row(df))
            }
          }
        }
      }

      def permutations(list:AxisValueList[AxisValue]):List[AxisValueList[AxisValue]] = {
        (list.list.size to 0 by -1).map(col => {
          val (start,end) = list.list.splitAt(col)
          AxisValueList(start ::: end.map(c => if (c.isMeasure) c else c.toTotal))
        }).toList.distinct
      }

      (Map() ++ mainTableBucket).foreach {
        case ((row, column), sum) => {
          val columnPermutations = permutations(column)
          val rowPermutations = permutations(row)
          for (rowP <- rowPermutations;
               colP <- columnPermutations) {
            val key = (rowP, colP)
            if (key != (row, column)) {
              val current = mainTableBucket.getOrElseUpdate(key, new DataFieldTotal)
              current.mutatingCombineGroup(sum)
            }
          }
        }
      }

      val allColumnValues = columnAxisValues.toList ::: mainTableBucket.map(_._1._2).toList
      val maxSize = if (allColumnValues.isEmpty) 0 else allColumnValues.maximum { col => col.list.size }
      val nonJaggedMainTableBucket = mainTableBucket.map { case ((row,col),tableCell) => {
        val filledCol = {
          val numElementsNeeded = maxSize - col.list.size
          val nullValue = if (col.list.nonEmpty) {
            AxisValue.Null.copy(position = col.list.head.position)
          } else {
            AxisValue.Null
          }
          AxisValueList(List.fill(numElementsNeeded)(nullValue) ::: col.list)
        }
        ((row,filledCol),tableCell)
      }}
      val nonJaggedColumnAxisValues = columnAxisValues.map(col => {
        val numElementsNeeded = maxSize - col.list.size
        val nullValue = if (col.list.nonEmpty) {
          AxisValue.Null.copy(position = col.list.head.position)
        } else {
          AxisValue.Null
        }
        AxisValueList(List.fill(numElementsNeeded)(nullValue) ::: col.list)
      })

      val rowGrid = (nonJaggedMainTableBucket.keySet.map(_._1) ++ rowAxisValues.flatMap(permutations).toSet).toList
      val colGrid = (nonJaggedMainTableBucket.keySet.map(_._2) ++ nonJaggedColumnAxisValues.flatMap(permutations).toSet).toList

      def createNodes(axisGrid:List[AxisValueList[AxisValue]]):List[AxisNode] = {
        val sorted = axisGrid.sortWith((a, b) => {
          var count = 0
          var result = false
          var running = true
          val aList = a.list
          val bList = b.list
          val maxSize = math.min(aList.size, bList.size)
          while (running && count < maxSize) {
            val aa = aList(count)
            val bb = bList(count)
            if (aa != bb) {
              result = aa <(bb, fieldDetailsLookup(bb.field).comparator)
              running = false
            }
            count += 1
          }
          result
        }).map(_.list.toArray).toArray
        AxisNodeBuilder.createNodes(sorted, 0, sorted.size, 0)
      }

      val rowAxis = createNodes(rowGrid)
      val columnAxis = createNodes(colGrid)

      val equalToReferenceTableCellMap = new HashMap[Any,Any]
      val equalToReferenceAxisValueListMap = new HashMap[AxisValueList[AxisValue],AxisValueList[AxisValue]]
      val equalToReferenceAxisValue = new HashMap[AxisValue,AxisValue]

      def compress(k1:AxisValueList[AxisValue]) = {
        val compressed = AxisValueList(k1.list.map( v => equalToReferenceAxisValue.getOrElseUpdate(v,v)))
        equalToReferenceAxisValueListMap.getOrElseUpdate(compressed,compressed)
      }

      val aggregatedMainBucket = nonJaggedMainTableBucket.map{case ((k1,k2),v) => {
        val cv = v.cellValue
        ((compress(k1),compress(k2)),
                equalToReferenceTableCellMap.getOrElseUpdate(cv,cv))}
      }

      val possibleValuesConvertedToTree = pivotResult.possibleValues.map {
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
                val (normalValues, nullValues) = unsortedValues.partition(_ != UndefinedValue)
                val arrayOfObjects = normalValues.toArray.asInstanceOf[Array[Object]]
                try {
                  val sorted = arrayOfObjects.sorted(fd.comparator)
                  nullValues ::: sorted.toList
                } catch {
                  case e =>
                    println("Invalid objects, could not sort")
                    arrayOfObjects.foreach{x => println(x + ", " + x.getClass)}
                    throw e
                }
              }
              field -> TreePivotFilter(TreePivotFilterNode("All", "All", values.map(v => TreePivotFilterNode(v, v.toString, List()))))
            }
          }
        }
      }

      def fieldIndexes(fields:List[Field]):Array[Int] = {
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

      val editableInfo = {
        dataSource.editable match {
          case None => None
          case Some(editPivot) => {
            // Work out if all the fields are in the appropriate places (rows or columns, or filter area with one selection)
            val editableMeasureFields = editPivot.editableToKeyFields.flatMap{case (editableField,keyFields) => {
              if (pivotState.columns.hasPathContaining(keyFieldsInColumnArea(keyFields, pivotState) + editableField)) {
                Some(editableField)
              } else {
                None
              }
            }}
            val editableKeyFields = editableMeasureFields.flatMap(field => {
              val keyFields = editPivot.editableToKeyFields(field)
              keyFields filter pivotState.rowFields.contains
            })
            val extraLine = editPivot.editableToKeyFields.map{case (editableField,keyFields) => {
              pivotState.columns.contains(editableField) && keyFieldsInColumnArea(keyFields, pivotState).isEmpty
            }}.exists(_ == true)

            val editableMeasures = Map() ++ editableMeasureFields.map(f => (f -> fieldDetailsLookup(f).parser))
            val editableKeys = Map() ++ editableKeyFields.map(f => (f -> fieldDetailsLookup(f).parser))

            Some(EditableInfo(editableMeasures, editableKeys, extraLine))
          }
        }
      }

      val formatInfo = FormatInfo(Map() ++ fieldDetailsLookup.map{case (f,fd) => (f -> fd.formatter)})

      // determine how long it runs for
      val now = System.currentTimeMillis
      Log.debug("Pivot Table Model generated in " + (now - then) + "ms")
      PivotTable(pivotState.rowFields, fieldIndexes(pivotState.rowFields), rowAxis, columnAxis, possibleValuesConvertedToTree,
        TreeDetails(treeDepths, maxDepths.toMap), editableInfo, formatInfo,
        Map() ++ aggregatedMainBucket.map { case ((r,c),v) => (r.list, c.list) -> v })
    }
  }

  private def keyFieldsInColumnArea(keyFields:Set[Field], pfs:PivotFieldsState) = {
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
      case 0 => TreePivotFilter(TreePivotFilterNode("None", "None", List()))
      case 1 => TreePivotFilter(merged.head.copy(children = merged))
      case _ => throw new Exception("Expect all paths to have a common root node: " + sortedPaths.map(_.path.head))
    }
  }
}

case class TreeDetails(treeDepths:Map[Field, (Int, Int)], maxTreeDepths:Map[Field, Int])

case class EditableInfo(editableMeasures:Map[Field,PivotParser], editableKeyFields:Map[Field,PivotParser], extraLine:Boolean)
case class FormatInfo(fieldToFormatter:Map[Field,PivotFormatter])
object FormatInfo {
  val Blank = FormatInfo(Map())
}