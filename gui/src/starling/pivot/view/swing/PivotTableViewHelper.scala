package starling.pivot.view.swing

import starling.browser.common.GuiUtils._
import starling.pivot.PivotFieldsState
import swing.Swing._
import java.awt.event.{AdjustmentListener, AdjustmentEvent}
import javax.swing._
import swing.event.{MousePressed, MouseClicked, MouseReleased, MouseEntered, MouseExited}
import java.awt.{RenderingHints, Graphics2D, Dimension, Color}
import swing.{Component, RadioButton, CheckBox, Label}
import starling.gui.api.{ReportSpecificChoices, ReportSpecificOptions}
import starling.utils.Describable
import starling.browser.common.{CrazyScrollPane, ArrowButton, MigPanel}

abstract class ReportSpecificOptionsButtonPanel(constraints:String) extends MigPanel(constraints) {
  private val bkColour = background
  private val borderColour = bkColour.darker
  val arc = 6

  background = ClearColour
  opaque = false

  override protected def paintComponent(g:Graphics2D) = {
    val x = 1
    val y = 1
    val w = size.width - 3
    val h = size.height - 3

    val oldColor = g.getBackground
    g.setColor(borderColour)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.drawRoundRect(x,y,w,h,arc,arc)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    g.setColor(oldColor)
  }

  def resetButton
}


object PivotTableViewHelper {
  import Describable._

  def generateReportSpecificPanels(fieldsState:PivotFieldsState, reportSpecificOptions : ReportSpecificOptions, model:starling.pivot.model.PivotTableModel) = {
    val reportSpecificChoices = ReportSpecificChoices(fieldsState.reportSpecificChoices).withDefaults(reportSpecificOptions)

    require(reportSpecificChoices isSubsetOf reportSpecificOptions)

    reportSpecificOptions.options.map {
      case (label, choices) =>
        choices match {
          case List(first : Boolean, second : Boolean) if first != second => {
            new ReportSpecificOptionsButtonPanel("") {
              val maxHeight = {
                new Label(label) {font = GuiFieldFont}.preferredSize.height
              }
              val cb = new CheckBox {
                font = GuiFieldFont
                background = ClearColour
                opaque = false
                focusable = false
                text = label
                reactions += {
                  case swing.event.ButtonClicked(b) => {
                    val newPivotFieldState = fieldsState.addChoice(label -> selected)
                    if (newPivotFieldState != fieldsState) {
                      model.publishFieldStateUpdated(newPivotFieldState)
                    }
                  }
                }
                maximumSize = new Dimension(preferredSize.width, maxHeight)
              }

              add(cb)
              resetButton

              def resetButton = cb.selected = reportSpecificChoices(label).asInstanceOf[Boolean]
            }
          }
          case _ => {
            new ReportSpecificOptionsButtonPanel("") {
              val lab = new Label(label) {font = GuiFieldFont}
              val maxHeight = lab.preferredSize.height
              add(lab, "ay bottom")
              val buttons = choices.map(n => new RadioButton(n.toString) {
                background = new Color(0,0,0,0)
                opaque = false
                focusable = false
                font = font.deriveFont(GuiFieldFont.getSize2D - 1.0f)
                foreground = Color.BLUE
                reactions += {
                  case scala.swing.event.ButtonClicked(b) => {
                    val newPivotFieldState = fieldsState.copy(reportSpecificChoices = (fieldsState.reportSpecificChoices + (label -> n)))
                    if (newPivotFieldState != fieldsState) {
                      model.publishFieldStateUpdated(newPivotFieldState)
                    }
                  }
                }
                maximumSize = new Dimension(preferredSize.width, maxHeight)
                minimumSize = preferredSize
              })
              val bg = new scala.swing.ButtonGroup(buttons: _*)
              buttons.foreach(add(_))
              resetButton

              def resetButton {
                reportSpecificChoices.get(label).foreach { selectedLabel =>
                  buttons.find(_.text == selectedLabel).foreach(button => bg.select(button))
                }
              }
            }
          }
        }
    }.toList
  }

  def generateScrollPaneHolders(comp:JComponent) = {
    val mainTableScrollPane = new JScrollPane(comp)
    mainTableScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
    mainTableScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
    mainTableScrollPane.setBorder(BorderFactory.createMatteBorder(1,0,0,0,BorderColour))

    val mainTableVerticalScrollBar = mainTableScrollPane.getVerticalScrollBar
    mainTableVerticalScrollBar.setPreferredSize(new Dimension(0,0))
    val mainTableHorizontalScrollBar = mainTableScrollPane.getHorizontalScrollBar
    mainTableHorizontalScrollBar.setPreferredSize(new Dimension(0,0))

    val fakeScrollPane = new JScrollPane
    val fakeHScrollBar = fakeScrollPane.getHorizontalScrollBar
    fakeHScrollBar.setUnitIncrement(UnitIncrement)
    fakeHScrollBar.setBlockIncrement(BlockIncrement)
    fakeHScrollBar.setModel(mainTableHorizontalScrollBar.getModel)
    val fakeHScrollBarHolder = new MigPanel("insets 0") {
      border = scala.swing.Swing.MatteBorder(0,0,1,0,BorderColour)
      add(fakeHScrollBar, "push, grow")
    }
    val fakeVScrollBar = fakeScrollPane.getVerticalScrollBar
    fakeVScrollBar.setUnitIncrement(UnitIncrement)
    fakeVScrollBar.setBlockIncrement(BlockIncrement)
    fakeVScrollBar.setModel(mainTableVerticalScrollBar.getModel)
    val fakeVScrollBarHolder = new MigPanel("insets 0") {
      border = scala.swing.Swing.MatteBorder(1,0,0,1,BorderColour)
      add(fakeVScrollBar, "push, grow")
    }
    (mainTableScrollPane, fakeHScrollBarHolder, fakeVScrollBarHolder)
  }

  def generateScrollableFieldChooser(comp:Component, scrollingNotifier:{def scrolling()}, tableView:PivotTableView) = {
    val leftButton = new ArrowButton(true)
    val rightButton = new ArrowButton(false)
    new CrazyScrollPane(comp, leftButton, rightButton) {
      opaque = false
      background = ClearColour
      border = EmptyBorder
      peer.getViewport.setOpaque(false)
      peer.getViewport.setBackground(ClearColour)

      // We need to hack about with the vertical scroll bar to ensure that the scroll pane behaves properly.
      verticalScrollBarPolicy = swing.ScrollPane.BarPolicy.Always
      verticalScrollBar.preferredSize = new Dimension(30,5)
      verticalScrollBar.minimumSize = new Dimension(30,5)
      verticalScrollBar.peer.setSize(new Dimension(30,5))
      verticalScrollBar.visible = false

      horizontalScrollBarPolicy = swing.ScrollPane.BarPolicy.AsNeeded
      horizontalScrollBar.preferredSize = new Dimension(0,0)
      horizontalScrollBar.unitIncrement = 40
      horizontalScrollBar.peer.addAdjustmentListener(new AdjustmentListener{
        def adjustmentValueChanged(e: AdjustmentEvent) {
          scrollingNotifier.scrolling()
        }
      })

      val buttons = horizontalScrollBar.peer.getComponents
      val rButton = buttons(0).asInstanceOf[JButton]
      val rButtonListeners = rButton.getMouseListeners
      val lButton = buttons(1).asInstanceOf[JButton]
      val lButtonListeners = lButton.getMouseListeners

      reactions += {
        case m @ MouseClicked(`leftButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(lButton)
          lButtonListeners.foreach(_.mouseClicked(e))
        }
        case m @ MousePressed(`leftButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(lButton)
          lButtonListeners.foreach(_.mousePressed(e))
        }
        case m @ MouseReleased(`leftButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(lButton)
          lButtonListeners.foreach(_.mouseReleased(e))
        }
        case m @ MouseClicked(`rightButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(rButton)
          rButtonListeners.foreach(_.mouseClicked(e))
        }
        case m @ MousePressed(`rightButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(rButton)
          rButtonListeners.foreach(_.mousePressed(e))
        }
        case m @ MouseReleased(`rightButton`,_,_,_,_) => {
          val e = m.peer
          e.setSource(rButton)
          rButtonListeners.foreach(_.mouseReleased(e))
        }
        case m @ MouseEntered(`leftButton`,_,_) => {
          if (tableView.fieldBeingDragged) {
            val e = m.peer
            e.setSource(lButton)
            lButtonListeners.foreach(_.mousePressed(e))
          }
        }
        case m @ MouseExited(`leftButton`,_,_) => {
          if (tableView.fieldBeingDragged) {
            val e = m.peer
            e.setSource(lButton)
            lButtonListeners.foreach(_.mouseReleased(e))
            lButtonListeners.foreach(_.mouseClicked(e))
          }
        }
        case m @ MouseEntered(`rightButton`,_,_) => {
          if (tableView.fieldBeingDragged) {
            val e = m.peer
            e.setSource(rButton)
            rButtonListeners.foreach(_.mousePressed(e))
          }
        }
        case m @ MouseExited(`rightButton`,_,_) => {
          if (tableView.fieldBeingDragged) {
            val e = m.peer
            e.setSource(rButton)
            rButtonListeners.foreach(_.mouseReleased(e))
            rButtonListeners.foreach(_.mouseClicked(e))
          }
        }
      }
      listenTo(leftButton.mouse.clicks, rightButton.mouse.clicks, leftButton.mouse.moves, rightButton.mouse.moves)
    }
  }
}