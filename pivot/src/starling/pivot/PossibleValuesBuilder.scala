package starling.pivot

import collection.Seq
import collection.immutable.Map
import collection.mutable.{Map => MutableMap, Set => MutableSet}
import model.UndefinedValue
import scalaz.Scalaz._
import starling.utils.ImplicitConversions._


class PossibleValuesBuilder(val allFields: Seq[FieldDetails], val filtersList: FiltersList,
                            initialPossibleValues: Map[Field, List[Any]] = Map()) {

  private val fieldDetailsMap = allFields.toMapWithKeys(_.field)
  private val possibleValues = {
    val values = MutableMap[Field, MutableSet[Any]]()

    (filtersList.fields ++ initialPossibleValues.keySet).foreach { values(_) = MutableSet[Any]() }
    initialPossibleValues.foreach { case (field, valuesForField) => values(field) ++= valuesForField }

    values
  }

  def +=(row : Map[Field,Any]) {
    def getFieldValue(field : Field, isForPossibleValues: Boolean) : Any = {
      val value = PivotValue.extractValue(row, field)

      if (isForPossibleValues) fieldDetailsMap(field).transformValueForGroupByField(value) else value
    }

    // Need to add values for all matching selections and the first non-matching (if that exists)
    for (filters <- filtersList) {
      val (matching, nonMatching) = filters.span { case (field, possibleValuesFilter) =>
        fieldDetailsMap.get(field).fold(selectionMatches(possibleValuesFilter, _, getFieldValue(field, false)), false)
      }
      matching.foreach {
        case (field, MeasurePossibleValuesFilter(_)) =>
        case (field, _) => possibleValues(field) += getFieldValue(field, true)
      }
      nonMatching match {
        case Nil =>
        case (field, MeasurePossibleValuesFilter(_)) :: _ =>
        case (field, _) :: _ => if (fieldDetailsMap.contains(field)) possibleValues(field) += getFieldValue(field, true)
      }
    }
  }

  def build:Map[Field, List[Any]] = possibleValues.mapValues(_.toList).toMap

  def selectedValues[T: Manifest](field: Field, selection: SomeSelection): List[Option[T]] = naValue[T](selection) ::: {
    fieldDetailsMap.get(field) match {
      case Some(fd) => {
        val possibleValuesForField: List[T] = possibleValues(field).toList.filterCast[T]
        val valuesToLabels: Map[T, LabeledFilterSelection] = fd.valuesToGroup(possibleValuesForField).mapValues(_.toLabel)

        possibleValuesForField.collect { case value
          if valuesToLabels.get(value).fold(selection.matches(fd, _), false) || selection.matches(fd, value) => {
            Option(value)
          }
        }
      }

      case None => selection.values.toList.map(_.safeCast[T])
    }
  }

  private def selectionMatches(possibleValuesFilter: PossibleValuesFilter, fieldDetails: FieldDetails, value: Any): Boolean = {
    val res = possibleValuesFilter match {
      case SelectionPossibleValuesFilter(someSelection: SomeSelection) => {
        val sv = selectedValues[Any](fieldDetails.field, someSelection).flatten
        val r = sv.contains(value)
        r
      }
      case _ => possibleValuesFilter.matches(fieldDetails, value)
    }
    res
  }

  private def naValue[T](selection: SomeSelection) = if (selection.values.contains(UndefinedValue)) List(none[T]) else List()
}