package starling.gui.pages

import starling.gui.StarlingIcons
import starling.browser.internal.RootBrowserBundle
import java.awt.Dimension
import starling.browser._
import common.MigPanel
import service.StarlingGUIEvent
import swing.event.{Event, ButtonClicked}
import swing._
import collection.mutable.{Stack => MStack}
import starling.gui.osgi.StarlingBrowserBundle

case class EventViewerPage() extends Page {
  def text = "Event Viewer"
  override def icon = StarlingIcons.im("/icons/16x16_event.png")
  def bundle = StarlingBrowserBundle.BundleName
  def build(serverContext: String) = null
  type SC = String
  def createServerContext(sc: ServerContext) = ""
  def createComponent(context:PageContext, data:PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:Option[PreviousPageData]) = new EventViewerPageComponent(context)
}

class EventViewerPageComponent(context:PageContext) extends MigPanel() with PageComponent {

  val eventStack = new MStack[Event]()
  val eventListView = new ListView[Event](eventStack)
  val eventScrollPane = new ScrollPane(eventListView)

  def updateData() {
    eventListView.listData = eventStack
  }

  val sendEventButton = new Button("Send Test Event") {
    reactions += { case ButtonClicked(_) => context.submit(new SubmitRequest[Unit]() {
      def baseSubmit(serverContext: ServerContext) {
        serverContext.browserService.testEvent()
      }
    })}
  }
  val clearButton = new Button("Clear") {
    reactions += { case ButtonClicked(_) => {eventStack.clear(); updateData()}}
  }
  val buttonPanel = new MigPanel("insets 0") {
    add(sendEventButton, "sg, wrap")
    add(clearButton, "sg")
  }
  add(buttonPanel, "ay top")
  add(eventScrollPane, "gapleft unrel, push, grow")

  listenTo(context.remotePublisher)
  reactions += {
    case event:StarlingGUIEvent => {
      eventStack push event
      updateData()
    }
  }
}