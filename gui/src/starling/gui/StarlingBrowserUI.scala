package starling.gui

import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import net.miginfocom.swing.MigLayout
import org.jdesktop.jxlayer.JXLayer
import java.awt.{Color, BorderLayout, FlowLayout, Dimension}
import starling.pivot.view.swing.MigPanel
import swing.event.ButtonClicked
import starling.gui.GuiUtils._
import java.awt.event.{KeyEvent, InputEvent}
import swing._
import javax.swing.{ImageIcon, JPanel, JComponent, BorderFactory, KeyStroke}

class StarlingBrowserUI extends AbstractLayerUI[JComponent] {
  private val contentPanel = new JPanel(new MigLayout("")) {
    setBackground(GuiUtils.ClearColour)
    setVisible(false)
  }

  private val errorIcon = new Label {
    icon = StarlingIcons.icon("/icons/22x22/emblems/emblem-important.png")
  }
  private val questionIcon = new Label {
    icon = StarlingIcons.icon("/icons/128x128_question.png")
  }

  def setError(title:String, error:String, errorOK: => Unit) {
    val errorPanel = new MigPanel("") {
      border = BorderFactory.createLineBorder(BorderColour)
      val okButton = new Button {
        text = "OK"
        reactions += {
          case ButtonClicked(e) => {
            clearContentPanel
            errorOK
          }
        }
      }
      add(errorIcon)
      add(new Label(title), "wrap")
      add(new ScrollPane(new TextArea() {
        text = error
        editable = false
      }), "skip 1, wrap unrel")
      add(okButton, "spanx, split, al r, tag ok")
    }

    contentPanel.add(errorPanel.peer, "push, al c c")

    contentPanel.setVisible(true)
    contentPanel.revalidate
  }

  def setYesNoMessage(message:String, reason:String, onEvent:(Boolean) => Unit, windowMethods:WindowMethods) {
    val oldDefaultButton = windowMethods.getDefaultButton

    def clearUp(action:Boolean) {
      clearContentPanel
      onEvent(action)
      windowMethods.setDefaultButton(oldDefaultButton)
    }

    val yesNoPanel = new MigPanel("") {
      border = BorderFactory.createLineBorder(new Color(158,16,40), 2)
      val label = new Label(message) {
        font = font.deriveFont(java.awt.Font.BOLD)
      }
      val textArea = LabelTextArea(reason)
      val yesButton = new Button {
        text = "Yes"
        reactions += {
          case ButtonClicked(e) => {clearUp(true)}
        }
      }
      val noButton = new Button {
        text = "No"
        reactions += {
          case ButtonClicked(e) => {clearUp(false)}
        }
      }

      add(questionIcon, "spany")
      add(label, "pushx, growx, wrap unrel, w " + label.preferredSize.width)
      add(textArea, "skip 1, push, grow, wrap unrel")
      add(yesButton, "skip 1, split, al right, sg button")
      add(noButton, "al right, sg button")
    }

    yesNoPanel.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Escape")
    val escapeAction = Action("Escape") {clearUp(false)}
    yesNoPanel.peer.getActionMap.put("Escape", escapeAction.peer)

    contentPanel.add(yesNoPanel.peer, "push, al c c")
    contentPanel.setVisible(true)
    contentPanel.revalidate
    windowMethods.setDefaultButton(Some(yesNoPanel.yesButton))
    yesNoPanel.yesButton.requestFocusInWindow
  }

  def setContent(content:Component, cancelAction:Option[()=> Unit]) {
    content.peer.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "Escape")
    val escapeAction = Action("Escape") {
      cancelAction match {
        case None =>
        case Some(ac) => ac()
      }
    }
    content.peer.getActionMap.put("Escape", escapeAction.peer)
    contentPanel.add(content.peer, "push, al c c")
    contentPanel.setVisible(true)
    contentPanel.revalidate
  }

  def clearContentPanel {
    contentPanel.setVisible(false)
    contentPanel.removeAll
  }

  override def installUI(c: JComponent) = {
    super.installUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.setLayout(new BorderLayout)
    glassPane.add(contentPanel)
  }

  override def uninstallUI(c: JComponent) = {
    super.uninstallUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.remove(contentPanel)
    glassPane.setLayout(new FlowLayout)
  }
}