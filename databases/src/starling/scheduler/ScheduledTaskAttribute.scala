package starling.scheduler

import starling.pivot.HasLongText


case class ScheduledTaskAttribute(value: String, details: String*) {
  def longText = HasLongText(value, if (details.isEmpty) value else details.mkString("\n"))
}