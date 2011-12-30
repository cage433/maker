package starling.browser.internal

import org.jdesktop.jxlayer.plaf.AbstractLayerUI
import net.miginfocom.swing.MigLayout
import swing.event.ButtonClicked
import starling.browser.common.GuiUtils._
import javax.swing.{KeyStroke, JPanel, JComponent}
import java.awt.event.KeyEvent
import scala.swing._
import org.jdesktop.jxlayer.JXLayer
import java.awt.{FlowLayout, BorderLayout, Color, Dimension}
import starling.browser.common._
import starling.browser.util.BrowserStackTraceToString

class StarlingBrowserUI extends AbstractLayerUI[JComponent] {
  val topPanelInsets = GuiUtils.RightPanelValue + 41
  private val contentPanel = new JPanel(new MigLayout("", "[grow, al c]", topPanelInsets + ":" + (topPanelInsets + 120) + "[p]" + GuiUtils.RightPanelSpace + ":null:null:push")) {
    setOpaque(false)
    setVisible(false)
  }

  private val errorIcon = new Label {
    icon = BrowserIcons.icon("/icons/128x128_error.png")
  }
  private val questionIcon = new Label {
    icon = BrowserIcons.icon("/icons/128x128_question.png")
  }
  private val detailsDownIcon = BrowserIcons.icon("/icons/small_down_arrows.png")
  private val detailsUpIcon = BrowserIcons.icon("/icons/small_up_arrows.png")

  def setError(title:String, message:String, throwable:Option[Throwable], errorOK: => Unit) {
    val errorPanel = new MigPanel with RoundedBackground {
      border = RoundedBorder(Color.RED)
      background = Color.WHITE
      minimumSize = new Dimension(550, 100)
      maximumSize = new Dimension(550, Integer.MAX_VALUE)
      val okButton = new Button {
        text = "OK"
        reactions += {
          case ButtonClicked(e) => {
            clearContentPanel()
            errorOK
          }
        }
      }
      val titleLabel = new Label(title) {
        font = font.deriveFont(java.awt.Font.BOLD)
      }
      val messageScrollPane = new ScrollPane(new TextArea() {
        text = message
        editable = false
        background = Color.WHITE
        lineWrap = true
        peer.setCaretPosition(0)
      })

      add(errorIcon, "spany 3")
      add(titleLabel, "gaptop rel, spanx, wrap unrel")
      add(messageScrollPane, "gapleft 10, spanx, pushy, grow, wrap unrel")

      throwable match {
        case None => add(okButton, "spanx, split, al r, tag ok")
        case Some(t) => {
          val throwablePanel = new MigPanel {
            background = Color.WHITE
            visible = false
            val throwableTextArea = new TextArea {
              text = BrowserStackTraceToString.string(t)
              editable = false
              peer.setCaretPosition(0)
            }
            val throwableScrollPane = new ScrollPane(throwableTextArea)
            add(throwableScrollPane, "push, grow")
          }

          val detailsButton = new Button("Details") {
            var detailsShowing = false
            icon = detailsDownIcon

            reactions += {
              case ButtonClicked(_) => {
                throwablePanel.visible = !detailsShowing
                detailsShowing = !detailsShowing
                if (detailsShowing) {
                  icon = detailsUpIcon
                } else {
                  icon = detailsDownIcon
                }
              }
            }
          }

          add(detailsButton, "pushx, al r")
          add(okButton, "tag ok")
          add(throwablePanel, "newline, spanx, hidemode 3")
        }
      }
    }

    contentPanel.removeAll()
    addToContentPanel(errorPanel)
    errorPanel.okButton.requestFocusInWindow()
  }

  private def addToContentPanel(comp:Component) {
    contentPanel.add(comp.peer)
    contentPanel.setVisible(true)
    contentPanel.revalidate()
  }

  def setYesNoMessage(message:String, reason:String, onEvent:(Boolean) => Unit, windowMethods:WindowMethods, componentToFocus:Option[java.awt.Component]) {
    val oldDefaultButton = windowMethods.getDefaultButton

    def clearUp(action:Boolean) {
      clearContentPanel()
      onEvent(action)
      windowMethods.setDefaultButton(oldDefaultButton)
      componentToFocus.map(c => {
        val res = c.requestFocusInWindow() // TODO - this doesn't seem to be working when deleting a bookmark
        if (!res) {
          swing.Swing.onEDT(swing.Swing.onEDT({
            c.requestFocusInWindow()
          }))
        }
      })
    }

    val yesNoPanel = new MigPanel("") with RoundedBackground {
      border = RoundedBorder(Color.RED)
      background = Color.WHITE
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

    addToContentPanel(yesNoPanel)
    windowMethods.setDefaultButton(Some(yesNoPanel.yesButton))
    yesNoPanel.yesButton.requestFocusInWindow()
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
    addToContentPanel(content)
  }

  def clearContentPanel() {
    contentPanel.setVisible(false)
    contentPanel.removeAll()
  }

  override def installUI(c: JComponent) {
    super.installUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.setLayout(new BorderLayout)
    glassPane.add(contentPanel)
  }

  override def uninstallUI(c: JComponent) {
    super.uninstallUI(c)
    val layer = c.asInstanceOf[JXLayer[JComponent]]
    val glassPane = layer.getGlassPane
    glassPane.remove(contentPanel)
    glassPane.setLayout(new FlowLayout)
  }
}