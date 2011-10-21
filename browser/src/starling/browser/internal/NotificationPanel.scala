package starling.browser.internal

import starling.browser.LocalCache
import starling.browser.common.MigPanel
import starling.browser.common.TwoFixedImagePanel
import swing.Label
import java.awt.Color
import java.awt.Cursor
import swing.event.MouseClicked
import javax.swing.ImageIcon
import starling.browser.common.RoundedBorder
import starling.browser.LocalCacheKey
import starling.browser.service.StarlingGUIEvent
import swing.Swing._

object NotificationType extends Enumeration {
  type NotificationType = Value
  val Action, Message = Value
}

import NotificationType._

object NotificationKeys {
  val AllNotifications                    = new LocalCacheKey[List[Notification]]("AllNotifications")
  val UserNotifications                   = new LocalCacheKey[List[Notification]]("UserNotifications")
}

class NotificationPanel(frameWidth: => Int, cache:LocalCache, containerMethods:ContainerMethods) extends MigPanel("", "[p]push[p]") {
  visible = false
  border = MatteBorder(1,0,0,0,new Color(182,182,182))

  def closeAll()  {
    cache.removeAllUserNotifications()
    containerMethods.updateNotifications()
  }

  val closeButton = new TwoFixedImagePanel(
    BrowserIcons.im("/icons/close.png"),
    BrowserIcons.im("/icons/stop.png"),
    closeAll()) {
    tooltip = "Close notification panel"
  }

  def updateLayout() {notificationLabelPanel.updateLayout()}

  val notificationLabelPanel = new MigPanel("insets 0") {
    private def getWidthOfNotifications(notes:List[NotificationLabel]):Int = notes.map(_.preferredSize.width).sum
    private def generateLabels:List[NotificationLabel] = cache.userNotifications.map(new NotificationLabel(_))

    reactions += {
      case NotificationLabelClosed(nl) => {
        cache.removeUserNotification(nl.notification)
        containerMethods.updateNotifications()
      }
      case NotificationLabelAction(nl) => {
        nl.notification.action()
        cache.removeUserNotification(nl.notification)
        containerMethods.updateNotifications()
      }
    }

    def updateLayout() {
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
        revalidate()
        repaint()

        NotificationPanel.this.visible = true
      } else {
        NotificationPanel.this.visible = false
      }
    }
  }

  add(notificationLabelPanel, "push, grow")
  add(closeButton)
}

case class NotificationLabelClosed(source:NotificationLabel) extends StarlingGUIEvent
case class NotificationLabelAction(source:NotificationLabel) extends StarlingGUIEvent

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
    BrowserIcons.im("/icons/close.png"),
    BrowserIcons.im("/icons/stop.png"),
    {NotificationLabel.this.publish(NotificationLabelClosed(NotificationLabel.this))}) {
  }

  add(label, closeButton)
}

class Notification(val text:String, val icon:ImageIcon, val notificationType:NotificationType, action0: => Unit) {
  def action() {action0}
}
object Notification {
  def apply(text:String, icon:ImageIcon, notificationType:NotificationType, action: => Unit) =
    new Notification(text, icon, notificationType, action)
}
