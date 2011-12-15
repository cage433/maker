package starling.curves

import starling.daterange._
import starling.gui.api.EnvironmentSpecificationLabel


class EnvironmentSpecification(val observationDay: DayAndTime, val environmentRule: EnvironmentRule) {
  val label = EnvironmentSpecificationLabel(observationDay, environmentRule.label)

  override def equals(other: Any) = other match {
    case envSpec: EnvironmentSpecification =>
      observationDay.equals(envSpec.observationDay) && environmentRule.label.equals(envSpec.environmentRule.label)
    case _ => false
  }
}