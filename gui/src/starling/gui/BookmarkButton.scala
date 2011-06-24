package starling.gui

import api.{UserReportData, BookmarksUpdate, BookmarkLabel}
import pages.NavigationButton
import starling.pivot.view.swing.MigPanel
import javax.swing.event.{DocumentEvent, DocumentListener}
import swing._
import event.{MousePressed, ButtonClicked}
import java.awt.{Dimension, Color, Polygon, RenderingHints, KeyboardFocusManager}
import xstream.GuiStarlingXStream
import starling.rmi.StarlingServer
import starling.pivot.PivotLayout
import javax.swing.{JPopupMenu, BorderFactory}
import swing.Swing._

class BookmarkButton(currentBookmark: => Bookmark, context:PageContext) extends NavigationButton {
  val noBookmarkIcon = StarlingIcons.icon("/icons/22x22_empty_star.png")
  val noBookmarkIconCal = StarlingIcons.icon("/icons/22x22_empty_star_cal.png")
  val bookmarkedIcon = StarlingIcons.icon("/icons/22x22_bookmark.png")
  val bookmarkedIconCal = StarlingIcons.icon("/icons/22x22_bookmark_cal.png")

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

  val replacePanel = new MigPanel {
    border = BorderFactory.createLineBorder(new Color(158,16,40), 2)
    private val questionIcon = new Label {
      icon = StarlingIcons.icon("/icons/128x128_question.png")
    }
    val label = new Label("A bookmark already exists with that name") {
      horizontalAlignment = Alignment.Left
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val textArea = starling.gui.GuiUtils.LabelTextArea("Would you like to replace it?")
    val yesButton = new Button {
      text = "Yes"
      reactions += {
        case ButtonClicked(e) => {
          val bookmarkName = getText.trim()
          def saveBookmark(a:Unit) {
            val bookmark = GuiStarlingXStream.write(currentBookmark)
            context.submit(SaveBookmarkRequest(BookmarkLabel(bookmarkName, bookmark)))

            // This is a hack to save the report to the server as well as the bookmark.
            currentBookmark match {
              case rb:ReportBookmark => {
                def saveReport(a:Unit) {
                  val userReportData = rb.userReportData
                  val pivotLayout = rb.pivotPageState.pivotFieldParams.pivotFieldState match {
                    case None => throw new Exception("I should have a layout at this stage")
                    case Some(pfs) => PivotLayout("special-" + bookmarkName, pfs, true,
                      rb.pivotPageState.otherLayoutInfo, "special", Nil)
                  }
                  context.submit(SaveReportRequest(bookmarkName, userReportData, pivotLayout, true, true, true))
                }
                context.submit(DeleteReportRequest(bookmarkName), saveReport)
              }
              case _ =>
            }

            clearUp()
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

  val savePanel = new MigPanel {
    var oldDefaultButton:Option[Button] = None

    val infoIcon = new Label {
      icon = StarlingIcons.icon("/icons/128x128_info.png")
    }
    border = BorderFactory.createLineBorder(new Color(158,16,40), 2)
    val label = new Label("Please Enter the Bookmark Name") {
      font = font.deriveFont(java.awt.Font.BOLD)
    }
    val nameLabel = new Label("Bookmark Name:")
    val nameField = new TextField(20)

    val okButton = new Button {
      text = "OK"
      reactions += {
        case ButtonClicked(e) => {
          val bookmarkName = getText
          if (bookmarkName.nonEmpty) {
            val currentBookmarkNames = context.localCache.bookmarks.map(_.name.trim)
            if (!currentBookmarkNames.contains(bookmarkName)) {
              val bookmark = GuiStarlingXStream.write(currentBookmark)
              val bookmarkLabel = BookmarkLabel(bookmarkName, bookmark)
              context.submit(SaveBookmarkRequest(bookmarkLabel))

              currentBookmark match {
                case rb:ReportBookmark => {
                  val userReportData = rb.userReportData
                  val pivotLayout = rb.pivotPageState.pivotFieldParams.pivotFieldState match {
                    case None => throw new Exception("I should have a layout at this stage")
                    case Some(pfs) => PivotLayout("special-" + bookmarkName, pfs, true,
                      rb.pivotPageState.otherLayoutInfo, "special", Nil)
                  }
                  context.submit(SaveReportRequest(bookmarkName, userReportData, pivotLayout, true, true, true))
                }
                case _ =>
              }

              clearUp()
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
      if (currentBookmark.daySensitive) {
        icon = bookmarkedIconCal
      } else {
        icon = bookmarkedIcon
      }
      tooltip = "Clear this bookmark"
    } else {
      if (currentBookmark.daySensitive) {
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
    val bookmarks = context.localCache.bookmarks.map(_.bookmark)
    bookmarks.contains(currentBookmark)
  }

  reactions += {
    case ButtonClicked(b) => {
      if (knownBookmark0) {
        val bookmarkName = context.localCache.bookmarks.find(_.bookmark == currentBookmark).get.name
        context.submitYesNo("Delete Bookmark?",
          "Are you sure you want to delete the \"" + bookmarkName + "\" bookmark?",
          DeleteBookmarkRequest(bookmarkName), (u:Unit) => {false}, (u:Unit) => {})
      } else {
        val oldDefaultButton = context.getDefaultButton
        savePanel.oldDefaultButton = oldDefaultButton
        setSavePanel(false)
        context.setContent(holderPanel, Some(savePanel.clearUp))
        context.setDefaultButton(Some(savePanel.okButton))
        savePanel.nameField.requestFocusInWindow()
      }
    }
    case BookmarksUpdate(user, bookmarks) if user == context.localCache.currentUser.username => {
      refresh()
      repaint()
    }
  }
  listenTo(context.remotePublisher)
}

case class SaveBookmarkRequest(savedBookmark:BookmarkLabel) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {server.saveBookmark(savedBookmark)}
}

case class DeleteBookmarkRequest(name:String) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {server.deleteBookmark(name)}
}

case class SaveReportRequest(reportName:String, userReportData:UserReportData, pivotLayout:PivotLayout,
                             shouldSaveLayout:Boolean, shouldAssociateLayout:Boolean, showParameters:Boolean) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {
    server.saveUserReport(reportName, userReportData, showParameters)
    if (shouldSaveLayout && shouldAssociateLayout) {
      // This is the case where it is a custom layout so we want to save the layout and associate it with this report
      server.saveLayout(pivotLayout.copy(associatedReports = List(reportName)))
    } else if (shouldAssociateLayout) {
      // This is the case where the layout is already saved but we want to associate it with this report.
      server.deleteLayout(pivotLayout.layoutName)
      server.saveLayout(pivotLayout.copy(associatedReports = reportName :: pivotLayout.associatedReports))
    }
  }
}

case class DeleteReportRequest(reportName:String) extends SubmitRequest[Unit] {
  def submit(server:StarlingServer) {server.deleteUserReport(reportName)}
}

class BookmarkDropDownButton(context:PageContext) extends Button {
  tooltip = "Choose a bookmark to go to"
  focusable = false
  background = GuiUtils.ClearColour
  opaque = false

  maximumSize = new Dimension(10, Integer.MAX_VALUE)

  val bookmarkPanel = new BookmarksPanel(context)
  bookmarkPanel.background = Color.WHITE
  val popupMenu = new JPopupMenu()
  popupMenu.add(bookmarkPanel.peer)
  popupMenu.setBorder(LineBorder(GuiUtils.BorderColour))

  reactions += {
    case MousePressed(_,_,_,_,_) if enabled => {
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