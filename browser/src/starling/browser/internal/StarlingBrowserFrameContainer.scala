package starling.browser.internal

import collection.mutable.ListBuffer
import starling.browser._
import common.GuiUtils._
import common.{MigPanel, GuiUtils}
import service._
import com.thoughtworks.xstream.XStream
import swing.{CheckBox, Frame, Publisher}
import swing.event.ButtonClicked

trait ContainerMethods {
  def createNewFrame(fromFrame: Option[StarlingBrowserFrame])
  def closeFrame(frame: StarlingBrowserFrame)
  def closeAllFrames(fromFrame: StarlingBrowserFrame)
  def updateNotifications
}

object RootBrowserContext extends BrowserBundle {
  val xstream = new XStream()
  def bundleName = "Root"
  def marshal(obj: AnyRef) = xstream.toXML(obj)
  def unmarshal(text: String) = xstream.fromXML(text)
  def hotKeys = Nil
  def settings(pageContext:PageContext) = {
    def createGeneralPane(context:PageContext) = {
      val currentLiveSetting = context.getSetting(UserSettings.LiveDefault, false)
      new MigPanel("insets n n n 0", "[" + StandardLeftIndent + "][p]") {
        val defaultLiveCheckbox = new CheckBox("Default Live") {
          selected = currentLiveSetting
          reactions += {case ButtonClicked(_) => context.putSetting(UserSettings.LiveDefault, selected)}
        }

        add(LabelWithSeparator("General"), "spanx, growx, wrap")
        add(defaultLiveCheckbox, "skip 1")
        reactions += {
          case UserSettingUpdated(UserSettings.LiveDefault) => {
            val b = context.getSetting(UserSettings.LiveDefault, false)
            defaultLiveCheckbox.selected = b
          }
        }
        listenTo(context.remotePublisher)
      }
    }
    createGeneralPane(pageContext) :: Nil
  }
  def homeButtons(pageContext:PageContext) = Nil
}

class StarlingBrowserFrameContainer(serverContext: ServerContext, lCache: LocalCache, remotePublisher: Publisher,
                                    homePage: Page, userSettings: UserSettings, frameTitle: String, extraInfo:Option[String]) extends ContainerMethods {
  private val pageBuilder = new PageBuilder(serverContext)
  private val frames = new ListBuffer[StarlingBrowserFrame]

  def createNewFrame(fromFrame: Option[StarlingBrowserFrame]) = {
    val newFrame = new StarlingBrowserFrame(homePage, pageBuilder, lCache, userSettings, remotePublisher, this, extraInfo)
    frames += newFrame
    newFrame.title = frameTitle
    fromFrame match {
      case None => {
        import UserSettings._
        // Load the user settings.
        if (userSettings.settingExists(MainWindowBounds)) {
          // Need to check if the screen is on the screen.
          val frameBounds = userSettings.getSetting(MainWindowBounds)
          if (GuiUtils.onScreen(frameBounds)) {
            newFrame.peer.setBounds(frameBounds)
          } else {
            newFrame.pack
            newFrame.centerOnScreen
          }
        } else {
          newFrame.pack
          newFrame.centerOnScreen
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
      import UserSettings._
      userSettings.putSetting(MainWindowBounds, frame.bounds)
      for (f <- frames) f.visible = false
      serverContext.browserService.saveSettings(userSettings.toLabel(serverContext))
    } catch {
      case e => e.printStackTrace()
    }
  }

  def closeFrame(frame: StarlingBrowserFrame) = {
    if ((frames.size == 1) && (BrowserLauncher.numberOfClientsLaunched <= 1)) {
      // This is the last frame so ensure user userSettings are saved.
      closeAllFrames(frame)
    } else {
      // There are other frames left so just dispose of this frame.
      BrowserLauncher.numberOfClientsLaunched -= 1
      frames -= frame
      frame.visible = false
      frame.dispose
    }
  }

  def closeAllFrames(fromFrame: StarlingBrowserFrame) = {
    println("Shutting down")
    saveUserSettings(fromFrame)
    System.exit(0)
  }

  def updateNotifications = {
    frames.foreach(_.updateNotifications)
  }

  createNewFrame(None)
}
