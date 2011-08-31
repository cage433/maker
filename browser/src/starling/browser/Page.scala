package starling.browser

import common.{MigPanel, GuiUtils}
import internal._
import java.awt.image.BufferedImage
import service.internal.HeterogeneousMap
import service.{BrowserService, Version}
import swing.event.Event
import javax.swing.border.Border
import java.awt.{Graphics2D, Dimension}
import java.awt.{Component=>AWTComp}
import scala.swing.Swing._
import swing._
import util.BrowserStackTraceToString

/**
 * IMPORTANT - A page should usually be a case class with the whole page described by parameters passed into it. No other vals should exist
 * and everything else in the page class should be a def.
 */
trait Page {
  def bundle:String
  def icon:BufferedImage
  def text:String
  def shortText:String = text
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PageData]):PageComponent
  def build(serverContext:SC):PageData
  def latestPage(localCache:LocalCache):Page = this
  def bookmark(serverContext:SC):Bookmark = new PageBookmark(this)
  type SC
  def createServerContext(sc:ServerContext):SC
}

trait ServerContext {
  def username:String
  def lookup[T](klass:Class[T]):T
  def browserService:BrowserService
  def browserBundles:List[BrowserBundle]
  def version:Version
  def bundleForName(name:String) = (RootBrowserContext :: browserBundles).find(_.bundleName == name).getOrElse(
    throw new Exception("No browser bundle found with name " + name))
}

case class BrowserDay(year:Int, month:Int, dayOfMonth:Int)

trait Bookmark {
  def daySensitive:Boolean
  def createPage(day:Option[BrowserDay], serverContext:ServerContext, context:PageContext):Page
}

case class UserSettingUpdated(key:Key[_]) extends Event

case class BookmarkData(name:String, bookmark:Bookmark)

case class PageBookmark(page:Page) extends Bookmark {
  def daySensitive = false
  def createPage(day:Option[BrowserDay], serverContext:ServerContext, context:PageContext) = page
}

case class Modifiers(ctrl:Boolean, shift:Boolean)
object Modifiers {
  val None = Modifiers(false, false)
  def modifiersEX(keyModifiers:scala.swing.event.Key.Modifiers) = {
    val bothMask = scala.swing.event.Key.Modifier.Control | scala.swing.event.Key.Modifier.Shift
    if ((keyModifiers & bothMask) == bothMask) {
      Modifiers(true, true)
    } else if ((keyModifiers & scala.swing.event.Key.Modifier.Control) == scala.swing.event.Key.Modifier.Control) {
      Modifiers(true, false)
    } else if ((keyModifiers & scala.swing.event.Key.Modifier.Shift) == scala.swing.event.Key.Modifier.Shift) {
      Modifiers(false, true)
    } else {
      None
    }
  }
  def modifiers(keyModifiers:Int) = {
    val bothMask = java.awt.event.ActionEvent.CTRL_MASK | java.awt.event.ActionEvent.SHIFT_MASK
    if ((keyModifiers & bothMask) == bothMask) {
      Modifiers(true, true)
    } else if ((keyModifiers & java.awt.event.ActionEvent.CTRL_MASK) == java.awt.event.ActionEvent.CTRL_MASK) {
      Modifiers(true, false)
    } else if ((keyModifiers & java.awt.event.ActionEvent.SHIFT_MASK) == java.awt.event.ActionEvent.SHIFT_MASK) {
      Modifiers(false, true)
    } else {
      None
    }
  }
}

trait PageContext {
	def goTo(page:Page, modifiers:Modifiers=Modifiers.None, compToFocus:Option[AWTComp]=None)
  def createAndGoTo(buildPage:ServerContext=>Page, onException:PartialFunction[Throwable, Unit] = { case e:UnsupportedOperationException => {}}, modifiers:Modifiers=Modifiers.None, compToFocus:Option[AWTComp]=None)
  def submit[R](submitRequest:SubmitRequest[R], onComplete:R=>Unit=(r:R)=>(), keepScreenLocked:Boolean = false, awaitRefresh:R=>Boolean=(r:R)=>false)
  def submitYesNo[R](message:String, description:String, submitRequest:SubmitRequest[R], awaitRefresh:R=>Boolean, onComplete:R=>Unit, keepScreenLocked:Boolean = false)
  def clearCache()
  def setContent(content:Component, cancelAction:Option[()=> Unit])
  def setErrorMessage(title:String, error:String)
  def clearContent()
  def setDefaultButton(button:Option[Button])
  def getDefaultButton:Option[Button]
  def localCache:LocalCache
  def remotePublisher:Publisher
  def requestFocusInCurrentPage()
  def getSetting[T](key:Key[T])(implicit m:Manifest[T]):T
  def getSetting[T](key:Key[T], default: => T)(implicit m:Manifest[T]):T
  def getSettingOption[T](key:Key[T])(implicit m:Manifest[T]):Option[T]
  def putSetting[T](key:Key[T], value:T)(implicit m:Manifest[T])
  def bundles:List[BrowserBundle]
}

trait SubmitRequest[R] {
  def baseSubmit(serverContext:ServerContext):R
}

trait PageData

object LocalCache {
  val Version = new LocalCacheKey[Version]("Version")
  val CurrentUserName = new LocalCacheKey[String]("CurrentUserName")
  val AllUserNames = new LocalCacheKey[List[String]]("AllUserNames")
  val Bookmarks = new LocalCacheKey[List[BookmarkData]]("BookmarkData")
}

case class LocalCacheKey[T](description: String) {
  override def hashCode = description.hashCode

  override def equals(obj: Any) = obj match {
    case LocalCacheKey(desc) => desc == description
    case _ => false
  }
}

case class LocalCache(localCache:HeterogeneousMap[LocalCacheKey]) {
  import NotificationKeys._
  def version = localCache(LocalCache.Version)
  def currentUserName = localCache(LocalCache.CurrentUserName)
  def allUserNames = localCache(LocalCache.AllUserNames)

  def bookmarks = localCache(LocalCache.Bookmarks)

  def userNotifications = localCache(UserNotifications)
  def removeUserNotification(notification:Notification) {localCache(UserNotifications) = localCache(UserNotifications).filterNot(_ == notification)}
  def removeAllUserNotifications() {localCache(UserNotifications) = List()}
}

trait ComponentState
trait ComponentTypeState
trait TypeFocusInfo
trait PageComponent extends Component {
  def getBorder:Option[Border] = Some(MatteBorder(1, 0, 0, 0, GuiUtils.BorderColour))
  def restoreToCorrectViewForBack() {}
  def getState:Option[ComponentState] = None
  def setState(state:Option[ComponentState]) {}
  def resetDynamicState() {}
  def pageHidden() {}
  def pageShown() {}
  def getTypeState:Option[ComponentTypeState] = None
  def setTypeState(typeState:Option[ComponentTypeState]) {}
  def getTypeFocusInfo:Option[TypeFocusInfo] = None
  def setTypeFocusInfo(focusInfo:Option[TypeFocusInfo]) {}
  def pageResized(newSize:Dimension) {}
  def defaultComponentForFocus:Option[java.awt.Component] = None

  override def paintChildren(g:Graphics2D) {
    try {
      super.paintChildren(g)
    } catch {
      case e:Exception => {
        e.printStackTrace()
        peer.removeAll()
        peer.add(new ExceptionPageComponent("Exception during paint", e).peer, "push, grow")
        revalidate()
        repaint()
      }
    }
  }
}

class ExceptionPageComponent(errorType:String, t:Throwable)  extends MigPanel("") with PageComponent {
  private val stackTraceComponent = new ScrollPane(new TextArea() {
    text = errorType + "\n\n" + BrowserStackTraceToString.string(t)
    editable = false
    wordWrap = true
  })
  add(new Label() {
    icon = BrowserIcons.icon("/icons/22x22/emblems/emblem-important.png")
  })
  add(new Label("The previous action failed. Please contact a developer."), "wrap")
  add(stackTraceComponent, "skip 1, push, grow")
  onEDT(stackTraceComponent.peer.getViewport.setViewPosition(new Point(0,0)))
}