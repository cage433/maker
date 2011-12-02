package starling.browser.internal

import collection.mutable.ListBuffer
import HomePage.StarlingHomePage
import starling.browser._
import common.GuiUtils._
import common.{MigPanel, GuiUtils}
import starling.browser.service.internal.HeterogeneousMap
import com.thoughtworks.xstream.XStream
import swing.{CheckBox, Frame, Publisher}
import swing.event.ButtonClicked
import com.thoughtworks.xstream.io.xml.DomDriver
import java.awt.event.{KeyEvent, InputEvent}
import javax.swing.KeyStroke
import utilspage.UtilsPage
import java.util.prefs.Preferences
import java.awt.Rectangle

trait ContainerMethods {
  def createNewFrame(fromFrame: Option[StarlingBrowserFrame], startPage:Either[Page,(ServerContext => Page, PartialFunction[Throwable,Unit])])
  def closeFrame(frame: StarlingBrowserFrame)
  def closeAllFrames(fromFrame: StarlingBrowserFrame)
  def updateNotifications()
}

object RootBrowserBundle extends BrowserBundle {
  val xstream = new XStream(new DomDriver())
  def bundleName = "Root"
  def marshal(obj: AnyRef) = xstream.toXML(obj)
  def unmarshal(text: String) = xstream.fromXML(text)
  override def settings(pageContext:PageContext) = {
    def createGeneralPane(context:PageContext) = {
      new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
        val defaultLiveCheckbox = new CheckBox("Default Live") {
          selected = context.getSetting(UserSettings.LiveDefault, false)
          reactions += {case ButtonClicked(_) => {
            context.putSetting(UserSettings.LiveDefault, selected)
          }}
        }
        val showPageTimeCheckbox = new CheckBox("Show page time") {
          selected = context.getSetting(UserSettings.ShowPageTime, false)
          reactions += {case ButtonClicked(_) => {
            context.putSetting(UserSettings.ShowPageTime, selected)
          }}
        }

        add(LabelWithSeparator("General"), "spanx, growx, wrap")
        add(defaultLiveCheckbox, "skip 1, wrap")
        add(showPageTimeCheckbox, "skip 1")
        reactions += {
          case UserSettingUpdated(UserSettings.LiveDefault) => {
            val b = context.getSetting(UserSettings.LiveDefault, false)
            defaultLiveCheckbox.selected = b
          }
          case UserSettingUpdated(UserSettings.ShowPageTime) => {
            val b = context.getSetting(UserSettings.ShowPageTime, false)
            showPageTimeCheckbox.selected = b
          }
        }
        listenTo(context.remotePublisher)
      }
    }
    createGeneralPane(pageContext) :: Nil
  }
}

object StarlingBrowserFrameContainer {
  var numberOfFramesOpen = 0
}

class StarlingBrowserFrameContainer(serverContext: ServerContext, lCache: LocalCache, pageBuilder:PageBuilder,
                                    homePage: Page, userSettings: UserSettings, frameTitle: String, extraInfo:Option[String]) extends ContainerMethods {
  private val frames = new ListBuffer[StarlingBrowserFrame]

  val nodeName = "starling/rectangle"
  def loadWindowLocation:Option[Rectangle] = {
    if (!Preferences.userRoot().nodeExists(nodeName)) {
      None
    } else {
      val prefs = Preferences.userRoot().node(nodeName);
      val r = new Rectangle(
        prefs.getInt("x", 10),
        prefs.getInt("y", 10),
        prefs.getInt("width", 100),
        prefs.getInt("height", 100)
      )
      Some(r)
    }
  }
  def storeWindowLocation(rectangle:Rectangle) {
    val prefs = Preferences.userRoot().node(nodeName);
    prefs.putInt("x", rectangle.x)
    prefs.putInt("y", rectangle.y)
    prefs.putInt("width", rectangle.width)
    prefs.putInt("height", rectangle.height)
  }

  def createNewFrame(fromFrame: Option[StarlingBrowserFrame], startPage:Either[Page,(ServerContext => Page, PartialFunction[Throwable,Unit])]) {
    StarlingBrowserFrameContainer.numberOfFramesOpen += 1
//    val newFrame = new StarlingBrowserFrame(homePage, startPage0, pageBuilder, lCache, userSettings, this, serverContext.extraInfo)
    val newFrame = new StarlingBrowserFrame(homePage, startPage, pageBuilder, lCache, userSettings, this, extraInfo)
    frames += newFrame
    newFrame.title = frameTitle
    fromFrame match {
      case None => {
        import UserSettings._
        // Load the user settings.
        loadWindowLocation match {
          case Some(frameBounds) => {
            // Need to check if the screen is on the screen.
            if (GuiUtils.onScreen(frameBounds)) {
              newFrame.peer.setBounds(frameBounds)
            } else {
              newFrame.pack()
              newFrame.centerOnScreen()
            }
          }
          case None => {
            newFrame.pack()
            newFrame.centerOnScreen()
          }
        }
      }
      case Some(fFrame) => {
        // Display the new frame slightly below the other frame, with the same size.
        newFrame.peer.setBounds(fFrame.bounds.x + 20, fFrame.bounds.y + 20, fFrame.bounds.width, fFrame.bounds.height)
      }
    }
    newFrame.visible = true
  }

  private def saveUserSettings(frame: Frame) {
    try {
      storeWindowLocation(frame.bounds)
      for (f <- frames) f.visible = false
      serverContext.browserService.saveSettings(userSettings.toLabel(serverContext))
    } catch {
      case e => e.printStackTrace()
    }
  }

  def closeFrame(frame: StarlingBrowserFrame) {
    if ((frames.size == 1)  && (StarlingBrowserFrameContainer.numberOfFramesOpen <= 1)) {
      // This is the last frame so ensure user userSettings are saved.
      closeAllFrames(frame)
    } else {
      // There are other frames left so just dispose of this frame.
      StarlingBrowserFrameContainer.numberOfFramesOpen -= 1
      frames -= frame
      frame.visible = false
      frame.dispose()
    }
  }

  def closeAllFrames(fromFrame: StarlingBrowserFrame) {
    println("Shutting down")
    saveUserSettings(fromFrame)
    System.exit(0)
  }

  def updateNotifications() {
    frames.foreach(_.updateNotifications())
  }

  def showNewPage(page:Page) {
    // Check to see if the page already exists, if it does, go to it otherwise create a new page.
    frames.find(f => f.containsPage(page)) match {
      case Some(cf) => {
        cf.showPage(page)
        cf.peer.toFront()
        cf.repaint()
      }
      case None => {
        // Can't find so create a new tab on the last frame
        frames.last.showNewPage(page)
        frames.last.peer.toFront()
        frames.last.repaint()
      }
    }
  }

}
