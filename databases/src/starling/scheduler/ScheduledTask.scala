package starling.scheduler

import swing.event.Event

import starling.daterange.Day
import starling.pivot._
import starling.utils.ImplicitConversions._
import starling.utils.{Enableable, Broadcaster}
import starling.services.EmailService
import starling.gui.api.Email


trait ScheduledTaskAttributes {
  val DataSource = "DataSource"
  val DataSink   = "DataSink"
  val EmailFrom  = "EmailFrom"
  val EmailTo    = "EmailTo"
}

trait ScheduledTask extends ScheduledTaskAttributes with Enableable { self =>
  enable

  def attribute(name: String, alternative: String = ""): ScheduledTaskAttribute = attributes.getOrElse(name, ScheduledTaskAttribute(alternative))
  def attributes: Map[String, ScheduledTaskAttribute] = Map()
  def perform(observationDay: Day) { if (isEnabled) execute(observationDay) }
  protected def execute(observationDay: Day)

  protected def fields(names: String*) = names.map(Field(_)).toList
  protected def filters(filters: (String, Any)*): List[(Field, Selection)] =
    filters.toMap.mapKeys(Field(_)).mapValues(value => SomeSelection(Set(value))).toList
  protected def filterToString(filter: List[(Field, Selection)]) = {
    filter.toMap.mapKeys(_.name).mapValues(_.description).map("%s = %s" % _).mkString(", ")
  }

  def withAttributes(additionalAttributes: (String, ScheduledTaskAttribute)*) = new ScheduledTask {
    def execute(observationDay: Day) = self.execute(observationDay)
    override def attributes = self.attributes ++ additionalAttributes
  }

  def withSource(name: String, details: String*) = withAttributes(DataSource → ScheduledTaskAttribute(name, details : _*))
  def withSink(name: String) = withAttributes(DataSink → ScheduledTaskAttribute(name))
}

object ScheduledTask {
  object Null extends ScheduledTask {
    protected def execute(observationDay: Day) {}
  }
}

case class SimpleScheduledTask(task: Day => Unit) extends ScheduledTask {
  def execute(observationDay: Day) = task(observationDay)
}

abstract class BroadcastingScheduledTask(broadcaster: Broadcaster) extends ScheduledTask {
  final def execute(observationDay: Day) = eventFor(observationDay).map(broadcaster.broadcast)

  protected def eventFor(observationDay: Day): Option[Event]
}

abstract class EmailingScheduledTask(service: EmailService, template: Email) extends ScheduledTask {
  final protected def execute(observationDay: Day) = emailFor(observationDay).map(service.send)

  override def attributes =
    super.attributes + (EmailFrom → ScheduledTaskAttribute(template.from)) + (EmailTo → ScheduledTaskAttribute(template.to))

  protected def emailFor(observationDay: Day): Option[Email]
}