package starling.gui

import api.{EnvironmentRuleLabel, EnvironmentSpecificationLabel}
import pages.Revertable
import starling.daterange._
import swing.event.Event
import swing.Component
import utils.RichReactor._
import starling.browser.common.MigPanel


class EnvironmentSpecificationLabelChooser(environmentSpecificationLabel0: EnvironmentSpecificationLabel, rules:List[EnvironmentRuleLabel], enableFlags:Boolean = true)
  extends MigPanel("insets 0", "[p][p]") with Revertable {

  def revert() = this.suppressingSelf(environmentSpecification = environmentSpecificationLabel0)

  val dayChooser = new DayChooser(environmentSpecificationLabel0.observationDay.day, enableFlags)
  val ruleChooser = new EnvironmentRuleChooser(environmentSpecificationLabel0.environmentRule, rules)

  add(dayChooser, ruleChooser)

  reactions += {
    case DayChangedEvent(`dayChooser`, _) => publish(EnvironmentSpecificationLabelChangedEvent(this, environmentSpecification))
    case EnvironmentRuleLabelChangedEvent(`ruleChooser`, _) => publish(EnvironmentSpecificationLabelChangedEvent(this, environmentSpecification))
  }

  listenTo(dayChooser, ruleChooser)

  def day = dayChooser.day
  def day_=(day:Day) = dayChooser.day = day

  def rule = ruleChooser.rule
  def rule_=(rule:EnvironmentRuleLabel) = ruleChooser.rule = rule

  def environmentSpecification = EnvironmentSpecificationLabel(day.endOfDay, rule)
  def environmentSpecification_=(environmentSpecification: EnvironmentSpecificationLabel) = {
    day = environmentSpecification.observationDay.day;
    rule = environmentSpecification.environmentRule
  }

  def flagged = dayChooser.flagged
  def flagged_=(days:Set[Day]) = dayChooser.flagged = days

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    dayChooser.enabled = b
    ruleChooser.enabled = b
  }
}

case class EnvironmentSpecificationLabelChangedEvent(source: Component, environmentSpecificationLabel:EnvironmentSpecificationLabel) extends Event
