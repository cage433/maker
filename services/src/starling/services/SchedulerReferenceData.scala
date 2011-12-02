package starling.services

import starling.pivot._
import starling.scheduler.{Scheduler, ScheduledTaskAttributes}
import starling.daterange.Location


class SchedulerReferenceData(scheduler: Scheduler) extends UnfilteredPivotTableDataSource with ScheduledTaskAttributes {
  val group@List(name, timing, londonTime, period, calendar, producer, consumer, sender, recipients, enabled) = fieldDetails(
    "Task", "Starting Time", "London Time", "Period", "Calendar", "Producer", "Consumer", "Sender", "Receipients", "Enabled")

  val fieldDetailsGroups = List(FieldDetailsGroup("Schedule", group))
  override val initialState = DefaultPivotState(PivotFieldsState(rowFields = fields(name), dataFields = fields(group.tail)))

  def unfilteredData(pfs: PivotFieldsState) = scheduler.getTasks.map { task => fields(
    name       → task.name,
    timing     → task.time.prettyTime,
    londonTime → task.time.prettyTime("HH:mm", Location.London),
    period     → task.time.description,
    calendar   → task.cal.name,
    producer   → task.attribute(DataSource).longText,
    enabled    → task.isEnabled,
    consumer   → task.attribute(DataSink).longText,
    sender     → task.attribute(EmailFrom).longText,
    recipients → task.attribute(EmailTo).longText)
  }
}