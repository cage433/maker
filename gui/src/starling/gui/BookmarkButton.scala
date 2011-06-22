package starling.gui

import api.{UserReportData, BookmarksUpdate, BookmarkLabel}
import pages.NavigationButton
import swing.event.ButtonClicked
import starling.pivot.view.swing.MigPanel
import javax.swing.BorderFactory
import javax.swing.event.{DocumentEvent, DocumentListener}
import swing._
import java.awt.{Dimension, Color}
import xstream.GuiStarlingXStream
import starling.rmi.StarlingServer
import starling.pivot.PivotLayout

class BookmarkButton(currentBookmark: => Bookmark, pageContext:PageContext) extends NavigationButton {
//  val noBookmarkIcon = StarlingIcons.icon("/icons/22x22_empty_bookmark.png")
  val noBookmarkIcon = StarlingIcons.icon("/icons/22x22_empty_star.png")
  val bookmarkedIcon = StarlingIcons.icon("/icons/22x22_bookmark.png")

  def setSavePanel(setup:Boolean) {
    holderPanel.update(savePanel, setup)
    if (setup) {
      savePanel.nameField.requestFocusInWindow()
      savePanel.nameField.selectAll()
      pageContext.setDefaultButton(Some(savePanel.okButton))
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
            pageContext.submit(SaveBookmarkRequest(BookmarkLabel(bookmarkName, bookmark)))

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
                  pageContext.submit(SaveReportRequest(bookmarkName, userReportData, pivotLayout, true, true, true))
                }
                pageContext.submit(DeleteReportRequest(bookmarkName), saveReport)
              }
              case _ =>
            }

            clearUp()
          }
          pageContext.submit(DeleteBookmarkRequest(bookmarkName), saveBookmark)
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
            val currentBookmarkNames = pageContext.localCache.bookmarks.map(_.name.trim)
            if (!currentBookmarkNames.contains(bookmarkName)) {
              val bookmark = GuiStarlingXStream.write(currentBookmark)
              val bookmarkLabel = BookmarkLabel(bookmarkName, bookmark)
              pageContext.submit(SaveBookmarkRequest(bookmarkLabel))

              currentBookmark match {
                case rb:ReportBookmark => {
                  val userReportData = rb.userReportData
                  val pivotLayout = rb.pivotPageState.pivotFieldParams.pivotFieldState match {
                    case None => throw new Exception("I should have a layout at this stage")
                    case Some(pfs) => PivotLayout("special-" + bookmarkName, pfs, true,
                      rb.pivotPageState.otherLayoutInfo, "special", Nil)
                  }
                  pageContext.submit(SaveReportRequest(bookmarkName, userReportData, pivotLayout, true, true, true))
                }
                case _ =>
              }

              clearUp()
            } else {
              // Show a replace dialog.
              holderPanel.update(replacePanel, true)
              pageContext.setDefaultButton(Some(replacePanel.yesButton))
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
      pageContext.clearContent()
      pageContext.setDefaultButton(oldDefaultButton)
      pageContext.requestFocusInCurrentPage()
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
//      refresh
      add(c, "push,grow")
      revalidate()
      repaint()
    }
  }

  private var knownBookmark0 = false
  def knownBookmark_=(b:Boolean) {
    knownBookmark0 = b
    if (knownBookmark0) {
      icon = bookmarkedIcon
      tooltip = "Clear this bookmark"
    } else {
      icon = noBookmarkIcon
      tooltip = "Bookmark this page"
    }
  }
  def knownBookmark = knownBookmark0
  knownBookmark = false

  def refresh() {
    knownBookmark = checkIfBookmarkIsKnown
  }

  private def checkIfBookmarkIsKnown = {
    val bookmarks = pageContext.localCache.bookmarks.map(_.bookmark)
    bookmarks.contains(currentBookmark)
  }

  reactions += {
    case ButtonClicked(b) => {
      if (knownBookmark) {
        val bookmarkName = pageContext.localCache.bookmarks.find(_.bookmark == currentBookmark).get.name
        pageContext.submitYesNo("Delete Bookmark?",
          "Are you sure you want to delete the \"" + bookmarkName + "\" bookmark?",
          DeleteBookmarkRequest(bookmarkName), (u:Unit) => {false}, (u:Unit) => {})
      } else {
        val oldDefaultButton = pageContext.getDefaultButton
        savePanel.oldDefaultButton = oldDefaultButton
        setSavePanel(false)
        pageContext.setContent(holderPanel, Some(savePanel.clearUp))
        pageContext.setDefaultButton(Some(savePanel.okButton))
        savePanel.nameField.requestFocusInWindow()
      }
    }
    case BookmarksUpdate(user, bookmarks) if user == pageContext.localCache.currentUser.username => {
      refresh()
      repaint()
    }
  }
  listenTo(pageContext.remotePublisher)
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