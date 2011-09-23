package starling.browser.internal

import scala._
import swing._
import swing.event.{MousePressed, ButtonClicked}
import javax.swing.{JPopupMenu, BorderFactory}
import scala.swing.Swing._
import starling.browser.{ServerContext, SubmitRequest, PageContext, Bookmark}
import starling.browser.service.{BookmarksUpdate, BookmarkLabel}
import java.awt.{Polygon, Graphics2D, Dimension, Color, RenderingHints, KeyboardFocusManager}
import starling.browser.common.{RoundedBorder, RoundedBackground, GuiUtils, MigPanel}

class BookmarkButton(currentPage: CurrentPage, context:PageContext, pageBuilder:PageBuilder) extends NavigationButton {
  val noBookmarkIcon = BrowserIcons.icon("/icons/22x22_empty_star.png")
  val noBookmarkIconCal = BrowserIcons.icon("/icons/22x22_empty_star_cal.png")
  val bookmarkedIcon = BrowserIcons.icon("/icons/22x22_bookmark.png")
  val bookmarkedIconCal = BrowserIcons.icon("/icons/22x22_bookmark_cal.png")

  def setSavePanel(setup:Boolean) {
    holderPanel.update(savePanel, setup)
    if (setup) {
      savePanel.nameField.requestFocusInWindow()
      savePanel.nameField.selectAll()
      context.setDefaultButton(Some(savePanel.okButton))
    }
  }
  def setReplacePanel() {
    holderPanel.update(replacePanel, true)
  }
  private def getText:String = savePanel.nameField.text
  private def clearUp() {savePanel.clearUp()}

  val replacePanel = new MigPanel with RoundedBackground {
    border = RoundedBorder(Color.RED)
    background = Color.WHITE
    private val questionIcon = new Label {
      icon = BrowserIcons.icon("/icons/128x128_question.png")
    }
    val label = new Label("A bookmark already exists with that name") {
      horizontalAlignment = Alignment.Left
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val textArea = starling.browser.common.GuiUtils.LabelTextArea("Would you like to replace it?")
    val yesButton = new Button {
      text = "Yes"
      reactions += {
        case ButtonClicked(e) => {
          val bookmarkName = getText.trim()
          def saveBookmark(a:Unit) {
            val bundleName = currentPage.page.bundle
            val bundle = pageBuilder.bundleFor(bundleName)
            val bookmarkLabel = BookmarkLabel(bookmarkName, bundleName, bundle.marshal(currentPage.bookmark))
            context.submit(SaveBookmarkRequest(bookmarkLabel), (_:Unit) => clearUp, keepScreenLocked = true)
          }
          context.submit(DeleteBookmarkRequest(bookmarkName), saveBookmark)
        }
      }
    }
    val noButton = new Button {
      text = "No"
      reactions += {
        case ButtonClicked(e) => {setSavePanel(true)}
      }
    }

    add(questionIcon, "spany")
    add(label, "pushx, growx, wrap unrel, w " + label.preferredSize.width)
    add(textArea, "push, grow, wrap unrel")
    add(yesButton, "skip 1, split, al right, sg button")
    add(noButton, "al right, sg button")
  }

  val savePanel = new MigPanel with RoundedBackground {
    border = RoundedBorder(Color.RED)
    background = Color.WHITE
    var oldDefaultButton:Option[Button] = None

    val infoIcon = new Label {
      icon = BrowserIcons.icon("/icons/128x128_info.png")
    }
    val label = new Label("Please Enter the Bookmark Name") {
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val nameLabel = new Label("Bookmark Name:")
    val nameField = new TextField(20)

    val okButton = new Button {
      text = "OK"
      reactions += {
        case ButtonClicked(e) => {
          val realBookmarkName = getText.trim()
          val bookmarkName = realBookmarkName.toLowerCase
          if (bookmarkName.nonEmpty) {
            val currentBookmarkNames = context.localCache.bookmarks.map(_.name.trim.toLowerCase)
            if (!currentBookmarkNames.contains(bookmarkName)) {
              val bm = currentPage.bookmarkLabel(realBookmarkName)
              context.submit(SaveBookmarkRequest(bm), (_:Unit) => clearUp, keepScreenLocked = true)
            } else {
              // Show a replace dialog.
              holderPanel.update(replacePanel, true)
              context.setDefaultButton(Some(replacePanel.yesButton))
              replacePanel.yesButton.requestFocusInWindow()
            }
          }
        }
      }
    }
    val cancelButton = new Button {
      text = "Cancel"
      reactions += {case ButtonClicked(e) => {clearUp()}}
    }

    add(infoIcon, "spany")
    add(label, "spanx, wrap unrel")
    add(nameLabel, "gapleft unrel")
    add(nameField, "wrap unrel")
    add(okButton, "split, spanx, al right bottom, sg button")
    add(cancelButton, "al right bottom, sg button")

    def clearUp() {
      context.clearContent()
      context.setDefaultButton(oldDefaultButton)
      context.requestFocusInCurrentPage()
    }
  }

  val holderPanel = new MigPanel("insets 0") {
    opaque = false
    def update(c:Component, setSize:Boolean) {
      removeAll
      if (setSize) {
        val widthToUse = math.max(c.preferredSize.width, size.width)
        val heightToUse = math.max(c.preferredSize.height, size.height)
        c.preferredSize = new Dimension(widthToUse, heightToUse)
      }
      add(c, "push,grow")
      revalidate()
      repaint()
    }
  }

  private var knownBookmark0 = false
  def knownBookmark_=(b:Boolean) {
    knownBookmark0 = b
    if (knownBookmark0) {
      if (currentPage.bookmark.daySensitive) {
        icon = bookmarkedIconCal
      } else {
        icon = bookmarkedIcon
      }
      tooltip = "Clear this bookmark"
    } else {
      if (currentPage.bookmark.daySensitive) {
        icon = noBookmarkIconCal
        tooltip = "Bookmark this day sensitive page"
      } else {
        icon = noBookmarkIcon
        tooltip = "Bookmark this page"
      }
    }
  }
  def knownBookmark = knownBookmark0
  icon = noBookmarkIcon
  tooltip = "Bookmark this page"

  def refresh() {
    knownBookmark = checkIfBookmarkIsKnown
  }

  private def checkIfBookmarkIsKnown = {
    val bookmarks = context.localCache.bookmarks.flatMap(_.bookmark)
    bookmarks.contains(currentPage.bookmark)
  }

  reactions += {
    case ButtonClicked(b) => {
      if (knownBookmark0) {
        val bookmarkName = context.localCache.bookmarks.find(_.bookmark == Some(currentPage.bookmark)).get.name

        context.submitYesNo("Delete Bookmark?",
          "Are you sure you want to delete the \"" + bookmarkName + "\" bookmark?",
          DeleteBookmarkRequest(bookmarkName), awaitRefresh = (u:Unit) => {false}, onComplete = (u:Unit) => { clearUp _}, keepScreenLocked = false)
      } else {
        val oldDefaultButton = context.getDefaultButton
        savePanel.oldDefaultButton = oldDefaultButton
        setSavePanel(false)
        context.setContent(holderPanel, Some(() => savePanel.clearUp))
        context.setDefaultButton(Some(savePanel.okButton))
        savePanel.nameField.requestFocusInWindow()
      }
    }
    case BookmarksUpdate(user, bookmarks) if user == context.localCache.currentUserName => {
      refresh()
      repaint()
    }
  }
  listenTo(context.remotePublisher)
}

case class SaveBookmarkRequest(savedBookmark:BookmarkLabel) extends SubmitRequest[Unit] {

  def baseSubmit(serverContext: ServerContext) {serverContext.browserService.saveBookmark(savedBookmark)}
}

case class DeleteBookmarkRequest(name:String) extends SubmitRequest[Unit] {

  def baseSubmit(serverContext: ServerContext) { serverContext.browserService.deleteBookmark(name)}
}

class BookmarkDropDownButton(currentPage: CurrentPage, pageContext:PageContext) extends Button {
  tooltip = "Choose a bookmark to go to"
  focusable = false
  background = GuiUtils.ClearColour
  opaque = false

  maximumSize = new Dimension(10, Integer.MAX_VALUE)

  val bookmarkPanel = new BookmarksPanel(pageContext)
  bookmarkPanel.background = Color.WHITE
  val popupMenu = new JPopupMenu()
  popupMenu.add(bookmarkPanel.peer)
  popupMenu.setBorder(LineBorder(GuiUtils.BorderColour))

  reactions += {
    case MousePressed(_,_,_,_,_) if enabled => {
      bookmarkPanel.updateSelectedBookmark(currentPage.bookmark)
      val x = size.width - popupMenu.getPreferredSize.width
      popupMenu.show(peer, x, size.height-1)
      onEDT({
        KeyboardFocusManager.getCurrentKeyboardFocusManager.focusNextComponent(popupMenu)
      })
    }
    case GoingToBookmark => popupMenu.setVisible(false)
  }
  listenTo(mouse.clicks, bookmarkPanel)

  def col = if (enabled) GuiUtils.BorderColour else GuiUtils.DisabledBorderColour

  override protected def paintComponent(g:Graphics2D) {
    super.paintComponent(g)
    g.setColor(col)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val y = size.height / 2 - 1
    val triangle = new Polygon(Array(2,6,4), Array(y,y,y+3), 3)
    g.draw(triangle)
  }

  override protected def paintBorder(g:Graphics2D) {
    val h = size.height
    g.setClip(2, 0, size.width-2, h)
    super.paintBorder(g)
    g.setColor(col)
    g.setClip(0,0,2,h)
    g.drawLine(0,0,1,0)
    g.drawLine(0,h-1,1,h-1)
  }
}