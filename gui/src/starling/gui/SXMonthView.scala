package starling.gui

import org.jdesktop.swingx.event.DateSelectionEvent.EventType
import org.jdesktop.swingx.event.{DateSelectionEvent, DateSelectionListener}
import org.jdesktop.swingx.JXMonthView
import scala.swing.Swing._
import java.awt.Color
import swing.{TextField, Label, Component}
import java.awt.event.{ActionEvent, ActionListener}
import swing.event.{Event, SelectionChanged}
import java.util.Date
import starling.daterange.{TimeOfDay, Day}
import starling.browser.common.{MigPanel, GuiUtils}

class SXMonthView extends Component {
  override lazy val peer = new JXMonthView
  def traversable:Boolean = peer.isTraversable
  def traversable_=(traversable:Boolean) {peer.setTraversable(traversable)}
  def day:Day = Day.fromJavaDate(peer.getFirstSelectionDate)
  def day_=(day:Day) {peer.setSelectionDate(day.toTimestamp.toJavaDate)}
  def preferredColumnCount = peer.getPreferredColumnCount
  def preferredColumnCount_=(count:Int) = peer.setPreferredColumnCount(count)
  def firstDisplayedDay = peer.getFirstDisplayedDay
  def firstDisplayedDay_=(day:Day) = peer.setFirstDisplayedDay(day.toTimestamp.toJavaDate)
  def flagged:Set[Day] = Set() ++ peer.getFlaggedDates.toArray(Array[Date]()).map(Day.fromJavaDate)
  def flagged_=(days:Set[Day]) { peer.setFlaggedDates(days.toArray.map(_.toJavaDate) : _*) }
  def flaggedDayForeground:Color = peer.getFlaggedDayForeground
  def flaggedDayForeground_=(color:Color) { peer.setFlaggedDayForeground(color) }
  peer.getSelectionModel.addDateSelectionListener(new DateSelectionListener {
    def valueChanged(ev:DateSelectionEvent) = {
      if (ev.getEventType == EventType.DATES_SET) {
        publish(SelectionChanged(SXMonthView.this))
      }
    }
  })
  peer.addActionListener(new ActionListener {
    def actionPerformed(e:ActionEvent) = {
      e.getActionCommand match {
        case JXMonthView.CANCEL_KEY => publish(MonthViewCancelEvent(SXMonthView.this))
        case JXMonthView.COMMIT_KEY => publish(MonthViewCommitEvent(SXMonthView.this, day))
        case _ =>
      }
    }
  })
}

case class MonthViewCancelEvent(source:Component) extends Event
case class MonthViewCommitEvent(source:Component, day:Day) extends Event

class TitledDayPicker(title:Option[String] = None) extends MigPanel("insets 0") {

  private val dayField = new TextField {
    editable = false
  }
  val dayPicker = new SXMonthView {
    traversable = true
    border = LineBorder(GuiUtils.BorderColour)
  }
  def day:Day = dayPicker.day
  def day_=(day:Day) { dayPicker.day = day; dayField.text = day.toString }

  def dayAndTime = day.atTimeOfDay(timeOfDay)
  def timeOfDay:TimeOfDay = TimeOfDay.StartOfDay

  def flagged:Set[Day] = dayPicker.flagged
  def flagged_=(days:Set[Day]) {
    dayPicker.flaggedDayForeground = Color.BLACK
    dayPicker.foreground = Color.GRAY
    dayPicker.flagged = days
  }

  override def enabled = super.enabled
  override def enabled_=(b:Boolean) = {
    dayField.enabled = b
    dayPicker.enabled = b
  }

  listenTo(dayPicker)
  reactions += {
    case SelectionChanged(`dayPicker`) => {
      dayField.text = dayPicker.day.toString
      publish(SelectionChanged(TitledDayPicker.this))
    }
  }
  title match {
    case Some(text) => add(new Label(text), "wrap")
    case _ =>
  }
  add(dayField, "growx, gapbottom 2lp, wrap")
  add(dayPicker)
}