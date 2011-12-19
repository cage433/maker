package starling.gui

import swing._
import event._
import starling.daterange._


class TimeOfDayChooser(timeOfDay0: TimeOfDay = TimeOfDay.StartOfDay)
  extends TypedComboChooser[TimeOfDay](timeOfDay0, List(TimeOfDay.StartOfDay, TimeOfDay.EndOfDay), ListView.Renderer(_.shortName)) {

  protected def publish() : Unit = publish(TimeOfDayChangedEvent(this, timeOfDay, previousSelection.getOrElse(timeOfDay)))

  def timeOfDay : TimeOfDay = value
  def timeOfDay_=(timeOfDay:TimeOfDay) = value = timeOfDay
}

case class TimeOfDayChangedEvent(source: Component, timeOfDay: TimeOfDay, previousTimeOfDay:TimeOfDay) extends Event
