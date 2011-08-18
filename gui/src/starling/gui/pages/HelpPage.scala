package starling.gui.pages

import scala.io._

import starling.gui._
import java.awt._
import java.net._
import java.util._
import java.io._
import javax.swing._
import javax.swing.event._
import javax.swing.text.html._

import org.eclipse.mylyn.wikitext.core.parser.MarkupParser
import org.eclipse.mylyn.wikitext.core.parser.builder.HtmlDocumentBuilder
import org.eclipse.mylyn.wikitext.textile.core.TextileLanguage

import swing.EditorPane
import starling.browser.common.MigPanel
import starling.browser._
import internal.BrowserIcons

case object HelpPage extends StarlingServerPage {
  def text = "Help"
  def icon = StarlingIcons.im("/icons/16x16_Help.png")

  def createComponent(pageContext: PageContext, data: PageData, bookmark:Bookmark, browserSize: Dimension) = {
    val component = new WikiPageComponent(pageContext)
    val markup = Source.fromURL(getClass.getResource("/Help.txt")).getLines.mkString("\n")

    def storeImage(name:String, icon:ImageIcon) {
      component.storeImage(name, icon.getImage)
    }

    import StarlingIcons._
    storeImage("RowGrandTotals", StarlingIcons.RowGrandTotals)
    storeImage("ColumnTotals", StarlingIcons.ColumnTotals)
    storeImage("SubTotals", StarlingIcons.RowSubTotals)
    storeImage("Chart", StarlingIcons.Chart)
    storeImage("Copy", StarlingIcons.Copy)
    storeImage("Rotate", StarlingIcons.Rotate)
    storeImage("Lock", StarlingIcons.Lock)
    storeImage("SaveLayout", StarlingIcons.SaveLayout)
    storeImage("Calculate", StarlingIcons.Calculate)

    storeImage("SaveReportConfiguration", StarlingIcons.icon(SaveReportConfiguration))
    storeImage("MarketData", StarlingIcons.icon(MarketData))
    storeImage("Refresh", StarlingIcons.icon(BrowserIcons.Refresh))

    //component.registerPage("Home", StarlingHomePage) //Just an example FIXME_BROWSER

    component.setWikiText(markup)
    component.border = BorderFactory.createEmptyBorder(20, 20, 20, 20)
    val sp = new scala.swing.ScrollPane(component)
    new MigPanel("insets 0") with PageComponent {
      add(sp, "push,grow")
      override def getBorder = None
      override def defaultComponentForFocus = Some(this.peer)
    }
  }
  def build(reader:StarlingServerContext) = new PageData {}
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