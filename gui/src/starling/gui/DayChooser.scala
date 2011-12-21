package starling.gui

import swing._
import event._
import java.awt.image.BufferedImage
import swing.Swing._
import javax.swing.JPopupMenu
import starling.daterange._
import java.awt.{Dimension, KeyboardFocusManager, Cursor, Color}
import starling.browser.common._

class DayChooser(day0:Day = Day.today, enableFlags:Boolean = true, showDay:Boolean=false) extends MigPanel("insets 0", "[p]0[p]") {

  val enabledBorderColour = GuiUtils.BorderColour
  val disabledBorderColour = GuiUtils.DisabledBorderColour

  private var currentDay = day0

  private val minimumWidth = {
    val text = if (showDay) {
      "Mon 30 May"
    } else {
      "03 May"
    }
    val tmpLabel = new Label(text)
    tmpLabel.preferredSize.width + 7
  }

  def day = currentDay
  def day_=(d:Day) {
    val oldDay = currentDay
    currentDay = d
    dayField.text = if (showDay) d.toString("EEE dd MMM") else d.toString("dd MMM")
    dayField.monthView.day = d
    publish(DayChangedEvent(this, d, oldDay))
  }

  override def background_=(c:Color) {
    super.background = c
    dayField.background = c
    leftPanel.background = c
    rightPanel.background = c
    endPanel.background = c
  }

  val dayField = new TextField {
    editable = false
    cursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
    minimumSize = new Dimension(minimumWidth, preferredSize.height)

    override protected def paintBorder(g:Graphics2D) {
      super.paintBorder(g)
      val width = size.width - 1
      val height = size.height - 2
      g.setColor(background)
      g.drawLine(width, 1, width, height)
    }

    val monthView = new SXMonthView {
      traversable = true
      preferredColumnCount = 2
      val today = Day.today
      if (showPreviousMonth) {
        firstDisplayedDay = Month(today.year, today.month).previous.firstDay
      }
      if (enableFlags) {
        flaggedDayForeground = Color.BLACK
        foreground = Color.GRAY
      }
    }

    private def showPreviousMonth = {
      val today = Day.today
      today.dayNumber < 15
    }

    val popupMenu = new JPopupMenu
    popupMenu.add(monthView.peer)
    popupMenu.setBorder(LineBorder(GuiUtils.BorderColour))

    reactions += {
      case MouseClicked(_,_,_,_,_) => {
        if (popupMenu.isShowing) {
          popupMenu.setVisible(false)
        } else {
          showPopup()
        }
      }
      case KeyPressed(_, scala.swing.event.Key.Down, _, _) => showPopup()
      case MonthViewCommitEvent(`monthView`, d) => {
        popupMenu.setVisible(false)
        day = d
      }
      case MonthViewCancelEvent(`monthView`) => popupMenu.setVisible(false)
      case _ =>
    }

    listenTo(mouse.clicks, keys, monthView)

    def showPopup() {
      if (enabled) {
        val xPos = if (showPreviousMonth) {
          (0.0 - (monthView.preferredSize.width / 2.0)).toInt - 8
        } else {
          0
        }
        monthView.firstDisplayedDay = day
        popupMenu.show(peer, xPos, size.height-1)
        onEDT({
          KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
        })
      }
    }
  }

  def flagged_=(days:Set[Day]) {dayField.monthView.flagged = days}
  def flagged = dayField.monthView.flagged

  class ArrowPanel(constraints:String, image:BufferedImage, action: => Unit, focusOwner:Component) extends MigPanel(constraints) {
    val arrow = new FixedImagePanel(image)
    cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
    background = dayField.background
    add(arrow, "push, grow, al center")
    override protected def paintBorder(g:Graphics2D) {
      if (enabled) {
        g.setColor(enabledBorderColour)
      } else {
        g.setColor(disabledBorderColour)
      }
      val width = size.width - 1
      val height = size.height - 1
      g.drawLine(0, 0, width, 0)
      g.drawLine(0, height, width, height)
    }
    reactions += {
      case MouseClicked(_,_,_,_,false) => {
        if (enabled) action
        focusOwner.requestFocusInWindow()
      }
    }
    listenTo(mouse.clicks)

    override def enabled_=(b:Boolean) {
      super.enabled = b
      arrow.enabled = b
    }
  }

  val leftIcon = StarlingIcons.im("/icons/5x10_bullet-arrow-left.png")
  val rightIcon = StarlingIcons.im("/icons/5x10_bullet-arrow-right.png")

  def previousDay() {
    val d = dayField.monthView.day.previousWeekday
    day = d
  }

  def nextDay() {
    val d = dayField.monthView.day.nextWeekday
    day = d
  }

  val leftPanel = new ArrowPanel("insets 0 2lp 0 3lp", leftIcon, previousDay, dayField)
  val rightPanel = new ArrowPanel("insets 0 3lp 0 2lp", rightIcon, nextDay, dayField)

  val endPanel = new MigPanel("insets 0 0 0 2lp") {
    background = dayField.background
    override protected def paintBorder(g:Graphics2D) {
      if (enabled) {
        g.setColor(enabledBorderColour)
      } else {
        g.setColor(disabledBorderColour)
      }
      val width = size.width - 1
      val height = size.height - 1
      g.drawLine(0, 0, width, 0)
      g.drawLine(0, height, width, height)
      g.drawLine(width, 0, width, height)
    }
  }

  override def enabled_=(b:Boolean) {
    super.enabled = b
    dayField.enabled = b
    leftPanel.enabled = b
    rightPanel.enabled = b
    endPanel.enabled = b
  }

  add(dayField, "pushx, growx")
  add(leftPanel, "growy")
  add(rightPanel, "growy")
  add(endPanel, "growy")

  day = day0
}

case class DayChangedEvent(source: Component, day: Day, previousDay:Day) extends Event