package starling.services

import starling.pivot.HasLongText


case class ScheduledTaskAttribute(value: String, details: String*) extends HasLongText {
  val longText = if (details.isEmpty) value else details.mkString("\n")
  override def toString = value
}