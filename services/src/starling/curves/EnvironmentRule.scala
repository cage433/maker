package starling.curves

import starling.daterange._

import collection.immutable.List
import starling.utils.ImplicitConversions._
import starling.db.MarketDataReader
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}
import collection.mutable.{Map => MMap}

trait EnvironmentRule {
  val label: EnvironmentRuleLabel
  val pricingGroups: List[PricingGroup]
  def name: String = label.name
  def createEnv(observationDay: Day, marketDataReader: MarketDataReader): EnvironmentWithDomain
  def createNullAtomicEnvironment(observationDay: Day):NullAtomicEnvironment = throw new Exception("Not implemented for " + this)
}

class EnvironmentRules {
  private val rules = MMap[EnvironmentRuleLabel, EnvironmentRule]()

  def add(rule: EnvironmentRule) = {
    require(!rules.contains(rule.label), "Rule already exists (%s) with label: %s" % (rules(rule.label), rule.label))
    rules.update(rule.label, rule)
  }

  def forName(name: String): EnvironmentRule = forLabel(EnvironmentRuleLabel(name))
  def forLabel(rule: EnvironmentRuleLabel): EnvironmentRule =
    rules.getOrThrow(rule, "No such rule: %s, valid values: %s" % (rule, rules.keySet.mkString(", ")))

  def forPricingGroup(pricingGroup: PricingGroup) = rules.filterValues(_.pricingGroups.contains(pricingGroup)).values.toList
}