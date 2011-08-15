package starling.gui

import starling.pivot.view.swing.{TwoFixedImagePanel, MigPanel}
import javax.swing.ImageIcon
import javax.swing.border.{AbstractBorder}
import java.awt.{Graphics, Component, Color, Graphics2D, Insets, RenderingHints, Cursor}
import swing.{AbstractButton, Button, Label}
import swing.event.{Event, MouseClicked, ButtonClicked}

object NotificationType extends Enumeration {
  type NotificationType = Value
  val Action, Message = Value
}

import NotificationType._

class NotificationPanel(frameWidth: => Int, cache:LocalCache, containerMethods:ContainerMethods) extends MigPanel("", "[p]push[p]") {
  visible = false

  def closeAll = {
    cache.removeAllUserNotifications
    containerMethods.updateNotifications
  }

  val closeButton = new TwoFixedImagePanel(
    StarlingIcons.im("/icons/close.png"),
    StarlingIcons.im("/icons/stop.png"),
    closeAll) {
    tooltip = "Close notification panel"
  }
  
  def updateLayout = notificationLabelPanel.updateLayout
  
  val notificationLabelPanel = new MigPanel("insets 0") {
    private def getWidthOfNotifications(notes:List[NotificationLabel]):Int = notes.map(_.preferredSize.width).sum
    private def generateLabels:List[NotificationLabel] = cache.userNotifications.map(new NotificationLabel(_))

    reactions += {
      case NotificationLabelClosed(nl) => {
        cache.removeUserNotification(nl.notification)
        containerMethods.updateNotifications
      }
      case NotificationLabelAction(nl) => {
        nl.notification.action
        cache.removeUserNotification(nl.notification)
        containerMethods.updateNotifications
      }
    }

    def updateLayout {
      var nll = generateLabels
      removeAll

      if (nll.nonEmpty) {

        val fw = frameWidth
        var totalWidth = getWidthOfNotifications(nll)
        while ((totalWidth + 50) > fw) {
          nll = nll.reverse.tail.reverse
          totalWidth = getWidthOfNotifications(nll)
        }

        nll.foreach(nl => {
          listenTo(nl)
          add(nl)
        })
        revalidate
        repaint

        NotificationPanel.this.visible = true
      } else {
        NotificationPanel.this.visible = false
      }
    }
  }

  add(notificationLabelPanel, "push, grow")
  add(closeButton)
}

case class RoundedBorder(colour:Color = GuiUtils.BorderColour, borderInsetSize:Int = 1) extends AbstractBorder {
  override def paintBorder(c:Component, g:Graphics, x:Int, y:Int, width:Int, height:Int) = {
    val g2 = g.asInstanceOf[Graphics2D]

    val s = c.getSize()
    val w = s.width - 1
    val h = s.height - 1
    val arc = 6

    val oldColour = g2.getColor
    g2.setColor(colour)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.drawRoundRect(0,0,w,h,arc,arc)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    g2.setColor(oldColour)
  }

  override def getBorderInsets(c:Component, insets:Insets) = {
    insets.top = borderInsetSize
    insets.left = borderInsetSize
    insets.bottom = borderInsetSize
    insets.right = borderInsetSize
    insets
  }
  override def getBorderInsets(c:Component) = new Insets(borderInsetSize,borderInsetSize,borderInsetSize,borderInsetSize)
}

case class NotificationLabelClosed(source:NotificationLabel) extends Event
case class NotificationLabelAction(source:NotificationLabel) extends Event

class NotificationLabel(val notification:Notification) extends MigPanel("") {
  border = RoundedBorder()

  val label = new Label {
    icon = notification.icon
    notification.notificationType match {
      case Message => text = notification.text
      case Action => {
        text = "<html><u>" + notification.text + "</u></html>"
        foreground = Color.BLUE
        cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
      }
    }

    reactions += {
      case MouseClicked(_,_,_,_,_) => NotificationLabel.this.publish(NotificationLabelAction(NotificationLabel.this))
    }
    listenTo(mouse.clicks)
  }

  val closeButton = new TwoFixedImagePanel(
    StarlingIcons.im("/icons/close.png"),
    StarlingIcons.im("/icons/stop.png"),
    {NotificationLabel.this.publish(NotificationLabelClosed(NotificationLabel.this))}) {
  }

  add(label, closeButton)
}

class Notification(val text:String, val icon:ImageIcon, val notificationType:NotificationType, action0: => Unit) {
  def action = action0
}
object Notification {
  def apply(text:String, icon:ImageIcon, notificationType:NotificationType, action: => Unit) =
    new Notification(text, icon, notificationType, action)
}
