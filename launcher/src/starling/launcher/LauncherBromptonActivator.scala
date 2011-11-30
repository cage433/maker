package starling.launcher

import starling.manager.{BromptonContext, BromptonActivator}
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver
import starling.browser._
import common.MigPanel
import internal.BrowserIcons
import java.awt.Dimension
import swing.event.{ButtonClicked, Event}
import java.io.ByteArrayOutputStream
import swing._
import javax.swing.text.DefaultCaret
import javax.swing.KeyStroke
import java.awt.event.KeyEvent

trait StdOut {
  def readAll:Array[Byte]
  def publisher:Publisher
}

case class StdOutEvent(data:Array[Byte]) extends Event
class LauncherBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {
    val publisher = context.awaitService(classOf[Publisher])
    val stdOut = context.awaitService(classOf[StdOut])
    publisher.listenTo(stdOut.publisher)

    context.registerService(classOf[BrowserBundle], new BrowserBundle() {
      val xstream = new XStream(new DomDriver())
      def bundleName = "Launcher"
      def marshal(obj: AnyRef) = xstream.toXML(obj)
      def unmarshal(text: String) = xstream.fromXML(text)
      override def utilButtons(pageContext: PageContext) = {
        PageButton("StdOut", StdOutPage, BrowserIcons.im("/icons/32x32_event.png"), Some(KeyStroke.getKeyStroke(KeyEvent.VK_S,0))) :: Nil
      }

    })
  }
}

object StdOutPage extends Page {
  def bundle = null
  def text = "Std Out"
  def icon = BrowserIcons.im("/icons/16x16_utilities.png")
  def createComponent(context: PageContext, data: PageData, bookmark: Bookmark, browserSize: Dimension, previousPageData: Option[PreviousPageData]) = {
    new StdOutPageComponent(data.asInstanceOf[StdOutPageData].data, context)
  }
  def build(serverContext: ServerContext) = StdOutPageData(serverContext.lookup(classOf[StdOut]).readAll)
  type SC = ServerContext
  def createServerContext(sc: ServerContext) = sc
}

case class StdOutPageData(data:Array[Byte]) extends PageData

class StdOutPageComponent(data:Array[Byte], context:PageContext) extends MigPanel("insets n n 0 0", "[p]unrel[p]") with PageComponent {

  val textArea = new TextArea(new String(data))
  textArea.peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE) //auto scroll
  val textAreaScrollPane = new ScrollPane(textArea)
  val updates = new ByteArrayOutputStream

  val clearButton = new Button("Clear") {
    reactions += { case ButtonClicked(_) => { textArea.text = "" }}
  }
  val restoreButton = new Button("Restore") {
    reactions += { case ButtonClicked(_) => {
      textArea.text = ""
      textArea.append(new String(data))
      textArea.append(new String(updates.toByteArray))
    } }
  }
  val buttonPanel = new MigPanel("insets 0") {
    add(clearButton, "sg, wrap")
    add(restoreButton, "sg")
  }
  add(buttonPanel, "ay top")
  add(textAreaScrollPane, "push,grow")

  listenTo(context.remotePublisher)
  reactions += {
    case event:StdOutEvent => {
      val text = new String(event.data)
      updates.write(event.data)
      val somethingIsSelected = textArea.peer.getSelectedText != null
      textArea.append(text)
      if (!somethingIsSelected) {
        textArea.peer.setCaretPosition(textArea.peer.getDocument.getLength)
      }
    }
  }
}