package starling.gui.api

import starling.daterange.{DayAndTime, Day}


case class EnvironmentRuleLabel(name:String)

object EnvironmentRuleLabel {
  val RealTime = EnvironmentRuleLabel("Real Time")
  val COB = EnvironmentRuleLabel("COB")
  val Default = EnvironmentRuleLabel("Default")
  val AllCloses = EnvironmentRuleLabel("All Closes")
  val MostRecentCloses = EnvironmentRuleLabel("Most Recent Closes")
}

case class EnvironmentSpecificationLabel(observationDay:DayAndTime, environmentRule:EnvironmentRuleLabel)
