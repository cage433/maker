package starling.gui.api

import starling.daterange._
import starling.utils.{ImplicitConversions, Describable}
import ImplicitConversions._


class ReportSpecificChoices(protected val choices : Map[String, Any] = Map()) {
  def getOrElse[T](name : String, default : => T): T = choices.getOrElse(name, default).asInstanceOf[T]

  def apply(key : String) = choices(key)
  def get(key : String) = choices.get(key)

  def labels = choices.keys.toList

  def isSubsetOf(options : ReportSpecificOptions) : Describable[Boolean] = {
    val invalidLabels = labels \\ options.labels
    val invalidValues = labels.flatMap{ label =>
      (!options.valuesFor(label).contains(choices(label))).toOption(label, choices(label))
    }

    val labelsMessage = (!invalidLabels.isEmpty).toOption("labels: " + invalidLabels.quote.mkString(","))
    val valuesMessage = (invalidLabels.isEmpty && !invalidValues.isEmpty).toOption(
      "values: " + invalidValues.map(lv => "%s -> %s" % (lv._1, lv._2)).mkString(","))

    Describable(invalidLabels.isEmpty && invalidValues.isEmpty,
      "ReportSpecificChoices contains invalid " + (labelsMessage :: valuesMessage :: Nil).flatten.mkString(", ")
    )
  }

  def withDefaults(options : ReportSpecificOptions) = new ReportSpecificChoices(options.default ++ choices)

  override def equals(obj: Any) = obj match {
    case other:ReportSpecificChoices => choices == other.choices
    case _ => false
  }

  override def toString = "ReportSpecificChoices: " + choices
}


object ReportSpecificChoices {
  val MonthChoiceText = "M"
  val WeekChoiceText = "W"
  val DayChoiceText = "D"

  def apply(choices : (String, Any)*): ReportSpecificChoices = create(choices.toMap)
  def apply(choices : scala.collection.Map[String, Any]) = new ReportSpecificChoices(choices.toMap)
  def create(choices : Map[String,Any]) = {
    val richChoices = choices.map {
      case (name, value) => {
        val richValue = if (name == "Tenor") {
          value match {
            case DayChoiceText => Day
            case WeekChoiceText => Week
            case MonthChoiceText => Month
          }
        } else {
          value
        }
        name -> richValue
      }
    }
    new ReportSpecificChoices(richChoices)
  }
}