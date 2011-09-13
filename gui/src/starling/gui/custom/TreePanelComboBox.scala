package starling.gui.custom

import swing.Label
import starling.pivot.controller.TreePivotFilter
import starling.gui.StarlingIcons._
import starling.pivot.{AllSelection, SomeSelection, Selection}
import java.awt.{Color, GradientPaint, Graphics2D, RenderingHints, Dimension, KeyboardFocusManager}
import starling.pivot.view.swing.{DisplayPopupEvent}
import swing.event._
import swing.Swing._
import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import javax.swing.{UIManager, JPopupMenu}
import starling.browser.common.GuiUtils._
import starling.browser.common.{FixedImagePanel, MigPanel}

object TreePanelComboBox {
  val NONE = new Object {
    override def toString = "None"
  }
}

class TreePanelComboBox(initialValuesAndSelection:(TreePivotFilter, Selection)) extends MigPanel("insets 0", "[p]0[p]") {
  opaque = false
  background = ClearColour

  private var currentValuesAndSelection = initialValuesAndSelection
  def valuesAndSelection = currentValuesAndSelection

  def valuesAndSelection_=(vAndSel:(TreePivotFilter, Selection)) {
    currentValuesAndSelection = vAndSel
    treePanel.filterHelper.resetPopup(currentValuesAndSelection, None)
    val (textToUse, numberTextToUse) = getTextForSelection
    label.text = textToUse
    button.numberText = numberTextToUse
  }
  
  val treePanel = new TreePanel(currentValuesAndSelection)
  val popup = new JPopupMenu {
    add(treePanel.peer)
    addPopupMenuListener(new PopupMenuListener {
      def popupMenuCanceled(e:PopupMenuEvent) = {}
      def popupMenuWillBecomeInvisible(e:PopupMenuEvent) = {
        // Whenever the popup panel is hidden, ensure it represents the state of the page.
        treePanel.filterPanel.textField.text = ""
        treePanel.filterHelper.resetPopup(currentValuesAndSelection, None)
      }
      def popupMenuWillBecomeVisible(e:PopupMenuEvent) = {}
    })
  }

  def getTextForSelection = {
    val (_,selection) = currentValuesAndSelection
    selection match {
      case AllSelection => {(treePanel.treeComponent.rootNode.getUserObject.asInstanceOf[CheckBoxListElement].label,"")}
      case SomeSelection(selectedValues) if selectedValues.isEmpty => {(TreePanelComboBox.NONE.toString, "0")}
      case SomeSelection(selectedValues) => {
        // Need to use the label
        (selectedValues.toList.map(v => {
          val s = treePanel.filterHelper.valueToLabelMap.getOrElse(v, "Unknown:"+v)
          if (s.length == 0) " " else s
        }).mkString(","), selectedValues.size.toString)
      }
    }
  }

  val (textToUse, numberTextToUse) = getTextForSelection
  val label = new TreePanelComboBoxLabel(textToUse)
  val button = new DropDownButton(numberTextToUse)

  add(label, "push, grow")
  add(button, "grow")

  reactions += {
    case DisplayPopupEvent(`button`) => {
      // Find out where to display the popup.
      if (treePanel.preferredSize.width < 200) {
        treePanel.preferredSize = new Dimension(200, treePanel.preferredSize.height)
      }
      val yPos = button.size.height - 1
      val xPos = button.size.width - treePanel.preferredSize.width - 7
      treePanel.scrollToFirstSelectedNode
      popup.show(button.peer, xPos, yPos)
      onEDT({
        KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popup)
        treePanel.filterPanel.textField.requestFocusInWindow
      })
    }
    case CancelEvent(`treePanel`) => popup.setVisible(false)
    case FilterSelectionChanged(`treePanel`, sel) => {
      valuesAndSelection = (valuesAndSelection._1, sel)
      publish(FilterSelectionChanged(this, sel))
    }
  }
  listenTo(button, treePanel)

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    label.enabled = b
    button.enabled = b
  }
}

class TreePanelComboBoxLabel(text0:String) extends MigPanel("insets 4lp") {
  opaque = false
  background = ClearColour

  private var currentText = text0
  def text = currentText
  def text_=(t:String) = {
    currentText = t
    label.text = currentText
  }

  val label = new Label(currentText) {
    horizontalAlignment = scala.swing.Alignment.Left
    override def text_=(s:String) = {
      super.text = s
      tooltip = s
    }
  }
  add(label, "push, grow")

  override protected def paintComponent(g:Graphics2D) = {
    val x = 0
    val y = 0
    val w = size.width
    val h = size.height - 1

    val fx = x + 1
    val fy = y + 1
    val fw = w - 1
    val fh = h - 1
    val fa = GuiFieldArc - 2

    val paintToUse = if (enabled) new GradientPaint(x, y, GUIFieldTopColour, x, h, GUIFieldBottomColour) else DisabledComboBackground
    val oldPaint = g.getPaint
    g.setPaint(paintToUse)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.fillRoundRect(fx, fy, GuiFieldArc-1, fh, fa, fa)
    g.setColor(GuiFieldBorderColour)
    g.drawRoundRect(x, y, GuiFieldArc, h, GuiFieldArc, GuiFieldArc)

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    val arcBy2 = GuiFieldArc / 2
    g.setPaint(paintToUse)
    g.fillRect(arcBy2, fy, w-arcBy2, fh)
    g.setColor(GuiFieldBorderColour)
    g.drawLine(arcBy2, y, fw, y)
    g.drawLine(arcBy2, h, fw, h)
  }

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    label.enabled = b
    repaint
  }
}

class DropDownButton(numberText0:String) extends MigPanel("insets 4lp") {
  opaque = false
  background = ClearColour
  private var currentNumberText = numberText0
  def numberText = currentNumberText
  def numberText_=(t:String) {
    currentNumberText = t
    repaint
  }

  private var mouseIsOver = false
  private val downArrow = new FixedImagePanel(im("/icons/small_down_arrow.png"))

  add(downArrow, "al center, pushy, growy, gapright 0, gaptop 1lp")

  reactions += {
    case MouseClicked(_, _, _, _, _) if enabled => publish(DisplayPopupEvent(this))
    case MouseEntered(_, _, _) if enabled => mouseIsOver = true; repaint
    case MouseExited(_, _, _) if enabled => mouseIsOver = false; repaint
  }
  listenTo(mouse.moves, mouse.clicks)

  private val overColour = GUIFieldBottomColour.darker

  override protected def paintComponent(g: Graphics2D) = {
    val x = 0
    val y = 0
    val w = size.width
    val h = size.height - 1

    val fx = x + 1
    val fy = y + 1
    val fw = w - 1
    val fh = h - 1
    val fa = GuiFieldArc - 2

    val bottomColour = if (mouseIsOver) overColour else GUIFieldBottomColour
    val paintToUse = if (enabled) new GradientPaint(x, fy, GUIFieldTopColour, x, fh, bottomColour) else DisabledComboBackground
    val oldPaint = g.getPaint
    g.setPaint(paintToUse)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.fillRoundRect(w - GuiFieldArc, fy, GuiFieldArc - 1, fh, fa, fa)
    g.setColor(GuiFieldBorderColour)
    g.drawRoundRect(fw - GuiFieldArc, y, GuiFieldArc, h, GuiFieldArc, GuiFieldArc)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    val arcBy2 = GuiFieldArc / 2
    g.setPaint(paintToUse)
    g.fillRect(x, fy, w-arcBy2, fh)
    g.setColor(GuiFieldBorderColour)
    g.drawLine(x, y, fw - arcBy2, y)
    g.drawLine(x, h, fw - arcBy2, h)

    if (mouseIsOver) {
      g.drawLine(x,fy,x,fh)
    }

    val lm = GuiFieldFilterNumberFont.getLineMetrics(currentNumberText, g.getFontRenderContext)
    // Draw the number text manually.
    g.setFont(GuiFieldFilterNumberFont)
    g.setColor(if (currentNumberText != "0") GuiFieldFilterNumberColour else Color.RED)
    g.drawString(currentNumberText, 1,lm.getHeight.round)

    g.setPaint(oldPaint)
  }

  override def enabled_=(b:Boolean) = {
    super.enabled = b
    downArrow.enabled = b
    repaint
  }
}