package starling.gui.api

import starling.daterange.Day

case class EnvironmentRuleLabel(name:String)

object EnvironmentRuleLabel {
  val RealTime = EnvironmentRuleLabel("Real Time")
  val COB = EnvironmentRuleLabel("COB")
}

case class EnvironmentSpecificationLabel(observationDay:Day, environmentRule:EnvironmentRuleLabel)