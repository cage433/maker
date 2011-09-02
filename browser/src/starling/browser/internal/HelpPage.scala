package starling.browser.internal

import io.Source
import starling.browser.common.MigPanel
import swing.EditorPane
import java.net.URL
import starling.browser._
import javax.swing.{KeyStroke, BorderFactory, ImageIcon}
import javax.swing.event.{HyperlinkEvent, HyperlinkListener}
import java.awt.{Image, Dimension}
import java.util.{Hashtable, Dictionary}
import javax.swing.text.html.HTMLDocument
import java.io.StringWriter
import org.eclipse.mylyn.wikitext.core.parser.builder.HtmlDocumentBuilder
import org.eclipse.mylyn.wikitext.core.parser.MarkupParser
import org.eclipse.mylyn.wikitext.textile.core.TextileLanguage

case object HelpPage extends Page {

  def bundle = RootBrowserBundle.bundleName

  def build(serverContext: String) = null

  type SC = String

  def createServerContext(sc: ServerContext) = ""

  def text = "Help"
  def icon = BrowserIcons.im("/icons/16x16_Help.png")

  def createComponent(pageContext: PageContext, data: PageData, bookmark:Bookmark, browserSize:Dimension, previousPageData:scala.Option[PageData]) = {
    val component = new WikiPageComponent(pageContext)

    val markup = pageContext.bundles.flatMap(_.helpEntries.map(_.markup)).mkString("\n")

    pageContext.bundles.foreach(_.helpEntries.foreach { entry => {
      entry.icons.foreach{ case(name,icon) => component.storeImage(name, icon.getImage)}
    } })

    pageContext.bundles.foreach(_.helpEntries.foreach { entry => {
      entry.links.foreach{ case(name,page) => component.registerPage(name, page)}
    } })

    component.setWikiText(markup)
    component.border = BorderFactory.createEmptyBorder(20, 20, 20, 20)
    val sp = new scala.swing.ScrollPane(component)
    new MigPanel("insets 0") with PageComponent {
      add(sp, "push,grow")
      override def getBorder = None
      override def defaultComponentForFocus = Some(this.peer)
    }
  }
}

class WikiPageComponent(pageContext: PageContext) extends EditorPane {
  private val pageLinks = new scala.collection.mutable.HashMap[URL, Page]()
  private val base = "file:/FakeBase/"
  peer.setContentType("text/html")
  peer.setEditable(false)
  //the line below fixes scrolling with the arrow keys. without this the arrow keys move an invisible cursor
  Array("UP", "DOWN", "LEFT", "RIGHT").foreach {k => peer.getInputMap().put(KeyStroke.getKeyStroke(k), "none")}
  //peer.getInputMap(javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW).remove(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0, false))
  //peer.setMaximumSize(new Dimension(300, 1000))
  peer.addHyperlinkListener(new HyperlinkListener() {
    def hyperlinkUpdate(event: HyperlinkEvent) {
      val eventType = event.getEventType();
      if (eventType == HyperlinkEvent.EventType.ACTIVATED) {
        val url = event.getURL()
        val link = url.toString.substring(base.length)
        if (link.startsWith("#")) {
          peer.scrollToReference(link.substring(1)) //Not working
        } else {
          pageLinks.get(url) match {
            case Some(page) => pageContext.goTo(page)
            case None => println("No page registered for " + url)
          }
        }
      }
    }
  })

  private val imageCache = {
    var cache = peer.getDocument().getProperty("imageCache").asInstanceOf[Dictionary[URL, Image]];
    if (cache == null) {
      cache = new Hashtable[URL, Image]()
      peer.getDocument().putProperty("imageCache", cache)
    }
    cache
  }

  def setWikiText(text: String) {
    val textile = parse(text)
    peer.setText(textile)
    peer.getDocument().asInstanceOf[HTMLDocument].setBase(new URL(base))
  }

  private def parse(text: String) = {
    val writer = new StringWriter();
    val builder = new HtmlDocumentBuilder(writer)
    builder.setEmitAsDocument(false) //stops output of <html> and <body> tags
    val parser = new MarkupParser(new TextileLanguage())
    parser.setBuilder(builder)
    parser.parse(text)
    writer.toString()
  }

  private def url(name: String) = {
    new URL(base + name)
  }

  def storeImage(name: String, image: Image) {
    imageCache.put(url(name), image);
  }

  def registerPage(name: String, page: Page) {
    pageLinks(url(name)) = page
  }
}