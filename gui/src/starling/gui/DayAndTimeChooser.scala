package starling.gui

import starling.daterange._
import swing.event.Event
import swing.Component
import starling.browser.common.MigPanel

class DayAndTimeChooser(day0:Day = Day.today, timeOfDay0:TimeOfDay = TimeOfDay.StartOfDay, enableFlags:Boolean = true)
  extends MigPanel("insets 0", "[p]") {

  val dayChooser = new DayChooser(day0, enableFlags)
  val timeOfDayChooser = new TimeOfDayChooser(timeOfDay0)

  add(dayChooser)
  add(timeOfDayChooser, "hidemode 3")

  reactions += {
    case DayChangedEvent(`dayChooser`, _) => publish(DayAndTimeChangedEvent(this, dayAndTime))
    case TimeOfDayChangedEvent(`timeOfDayChooser`, _) => publish(DayAndTimeChangedEvent(this, dayAndTime))
  }

  listenTo(dayChooser, timeOfDayChooser)

  def day = dayChooser.day
  def day_=(day:Day) = dayChooser.day = day

  def timeOfDay = timeOfDayChooser.timeOfDay
  def timeOfDay_=(timeOfDay:TimeOfDay) = timeOfDayChooser.timeOfDay = timeOfDay

  def dayAndTime = day.atTimeOfDay(timeOfDay)
  def dayAndTime_=(dayAndTime:DayAndTime) = {
    day = dayAndTime.day;
    timeOfDay = dayAndTime.timeOfDay
  }

  def flagged = dayChooser.flagged
  def flagged_=(days:Set[Day]) = dayChooser.flagged = days

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    dayChooser.enabled = b
    timeOfDayChooser.enabled = b
  }
}

case class DayAndTimeChangedEvent(source: Component, dayAndTime:DayAndTime) extends Event

