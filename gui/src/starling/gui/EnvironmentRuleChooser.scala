package starling.gui

import api.EnvironmentRuleLabel
import swing._
import event._
import starling.daterange._


class EnvironmentRuleChooser(rule0: EnvironmentRuleLabel, values: List[EnvironmentRuleLabel])
  extends TypedComboChooser[EnvironmentRuleLabel](rule0, values, ListView.Renderer(_.name)) {

  protected def publish() : Unit = publish(EnvironmentRuleLabelChangedEvent(this, rule))

  def rule : EnvironmentRuleLabel = value
  def rule_=(rule:EnvironmentRuleLabel) = value = rule
}

case class EnvironmentRuleLabelChangedEvent(source: Component, rule: EnvironmentRuleLabel) extends Event
