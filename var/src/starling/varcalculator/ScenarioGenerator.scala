package starling.varcalculator

import starling.curves.Environment


/** Scenario generators are used by var to produce multiple environments against which
 * 	to value instruments
 */
trait ScenarioGenerator{
  def originalEnv : Environment
  def next : Environment
  def originalBucketedEnv : Environment
}

