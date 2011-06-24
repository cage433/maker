package starling.gui

import api.BookmarksUpdate
import custom.{SXMonthView, TitledDayPicker}
import swing.Swing._
import starling.pivot.view.swing.{NListView, MigPanel}
import javax.swing.{JLabel, JList, DefaultListCellRenderer}
import java.awt.{Dimension, Color}
import swing.{Action, ScrollPane, ListView, Label}
import swing.event.{Event, SelectionChanged, KeyPressed, MouseClicked}
import starling.daterange.Day

class BookmarksPanel(context:PageContext) extends MigPanel("") {
  val iconLabel = new Label {
    icon = StarlingIcons.icon("/icons/32x32_report_star.png")
  }
  val textLabel = new Label("Select a bookmark (and observation day if required) to go to")
  val bookmarks = context.localCache.bookmarks

  val bookmarksListView = new NListView(bookmarks) {
    val bookmarkDataListCellRenderer = new DefaultListCellRenderer {
      val emptyBorder = EmptyBorder(0, 2, 0, 0)
      override def getListCellRendererComponent(list:JList, value:AnyRef, index:Int, isSelected:Boolean, cellHasFocus:Boolean) = {
        val l = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus).asInstanceOf[JLabel]
        val bookmarkData = value.asInstanceOf[BookmarkData]
        l.setText(bookmarkData.name)
        val iconToUse = if (bookmarkData.bookmark.daySensitive) {
          StarlingIcons.icon("/icons/10x10_calendar.png")
        } else {
          StarlingIcons.Blank10
        }
        l.setIcon(iconToUse)
        l.setBorder(emptyBorder)
        l
      }
    }
    renderer = ListView.Renderer.wrap(bookmarkDataListCellRenderer)
  }

  val dayPicker = new SXMonthView {
    border = LineBorder(GuiUtils.BorderColour)
    enabled = valuationDayShouldBeEnabled
    day = Day.today().previousWeekday
    traversable = true
  }
    
  def deleteBookmark() {
    val bookmarkSelected = bookmarksListView.selected
    context.submitYesNo("Delete Bookmark?",
      "Are you sure you want to delete the \"" + bookmarkSelected.name + "\" bookmark?",
      DeleteBookmarkRequest(bookmarkSelected.name), (u:Unit) => {false}, (u:Unit) => {})
  }

  def goToBookmark() {
    val bookmark = bookmarksListView.selected
    val baseDay = if (bookmark.bookmark.daySensitive) {
      Some(dayPicker.day)
    } else {
      None
    }
    publish(GoingToBookmark)
    context.createAndGoTo(server => {
      bookmark.bookmark.createPage(baseDay, server, context)
    })
  }

  val goToBookmarkAction = Action("Go"){goToBookmark()}
  goToBookmarkAction.toolTip = "Go to the selected bookmark (F9)"
  goToBookmarkAction.icon = NewPageButton.arrowImage

  val goToBookmarkButton = new NewPageButton {
    action = goToBookmarkAction
  }
  val bookmarkScrollPane = new ScrollPane(bookmarksListView)
  if (bookmarks.size <= 3) {
    bookmarkScrollPane.preferredSize = new Dimension(preferredSize.width, 100)
  }

  add(iconLabel, "split 3, spanx")
  add(textLabel)
  add(goToBookmarkButton, "gapbefore push, wrap")
  add(bookmarkScrollPane, "split, spanx, gapleft 10lp, push, grow")
  add(dayPicker)

  def componentsEnabled = false // Not used
  def componentsEnabled_=(b:Boolean) {
    goToBookmarkAction.enabled = b
    bookmarkScrollPane.enabled = b
    bookmarksListView.enabled = b
    dayPicker.enabled = b && valuationDayShouldBeEnabled
    iconLabel.enabled = b
    textLabel.enabled = b
  }

  def valuationDayShouldBeEnabled = {
    bookmarksListView.selectedOption match {
      case Some(bookmark) => bookmark.bookmark.daySensitive
      case _ => false
    }
  }

  reactions += {
    case BookmarksUpdate(username, _) if username == context.localCache.currentUser.username => {
      val newBookmarks = context.localCache.bookmarks
      val currentSelectedItem = bookmarksListView.selectedOption
      bookmarksListView.listData = newBookmarks
      if (bookmarksListView.listData.isEmpty) {
        componentsEnabled = false
      } else {
        componentsEnabled = true
      }
      bookmarksListView.selectedOption = currentSelectedItem
    }
    case MouseClicked(`bookmarksListView`,_,_,2,_) => {goToBookmark()}
    case KeyPressed(`bookmarksListView`, scala.swing.event.Key.Delete, _, _) => {deleteBookmark()}
    case SelectionChanged(`bookmarksListView`) => dayPicker.enabled = valuationDayShouldBeEnabled
  }
  listenTo(context.remotePublisher, bookmarksListView.keys, bookmarksListView.mouse.clicks, bookmarksListView.selection)

  if (!bookmarks.isEmpty) {
    bookmarksListView.selectIndices(0)
  }

  componentsEnabled = bookmarks.nonEmpty
}

case object GoingToBookmark extends Event