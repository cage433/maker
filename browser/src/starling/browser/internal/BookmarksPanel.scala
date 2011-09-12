package starling.browser.internal

import swing.Swing._
import java.awt.{Dimension, Color}
import swing.{Action, ScrollPane, ListView, Label, Component}
import swing.event.{Event, SelectionChanged, KeyPressed, MouseClicked}
import starling.browser.common._
import starling.browser.service.BookmarksUpdate
import org.jdesktop.swingx.JXMonthView
import java.util.{Calendar, GregorianCalendar, Date => JDate}
import starling.browser._
import javax.swing._
import java.awt.event.ActionEvent

class MinimalSXMonthView extends Component {
  override lazy val peer = new JXMonthView
  def traversable:Boolean = peer.isTraversable
  def traversable_=(traversable:Boolean) {peer.setTraversable(traversable)}
  def date:JDate= peer.getFirstSelectionDate
  def date_=(day:JDate) {peer.setSelectionDate(day)}
  def preferredColumnCount = peer.getPreferredColumnCount
  def preferredColumnCount_=(count:Int) = peer.setPreferredColumnCount(count)
  def firstDisplayedDay = peer.getFirstDisplayedDay
  def firstDisplayedDay_=(day:JDate) = peer.setFirstDisplayedDay(day)
//  peer.getSelectionModel.addDateSelectionListener(new DateSelectionListener {
//    def valueChanged(ev:DateSelectionEvent) = {
//      if (ev.getEventType == EventType.DATES_SET) {
//        publish(SelectionChanged(SXMonthView.this))
//      }
//    }
//  })
//  peer.addActionListener(new ActionListener {
//    def actionPerformed(e:ActionEvent) = {
//      e.getActionCommand match {
//        case JXMonthView.CANCEL_KEY => publish(MonthViewCancelEvent(SXMonthView.this))
//        case JXMonthView.COMMIT_KEY => publish(MonthViewCommitEvent(SXMonthView.this, day))
//        case _ =>
//      }
//    }
//  })
}

//case class MonthViewCancelEvent(source:Component) extends Event
//case class MonthViewCommitEvent(source:Component, day:Day) extends Event


class BookmarksPanel(context:PageContext) extends MigPanel("") {
  val iconLabel = new Label {
    icon = BrowserIcons.icon("/icons/32x32_report_star.png")
  }
  val textLabel = new Label("Select a bookmark (and observation day if required) to go to ")
  val bookmarks = context.localCache.bookmarks

  val bookmarksListView = new NListView(bookmarks) {
    val bookmarkDataListCellRenderer = new DefaultListCellRenderer {
      val emptyBorder = EmptyBorder(0, 2, 0, 0)
      override def getListCellRendererComponent(list:JList, value:AnyRef, index:Int, isSelected:Boolean, cellHasFocus:Boolean) = {
        val l = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus).asInstanceOf[JLabel]
        val bookmarkData = value.asInstanceOf[BookmarkData]
        l.setText(bookmarkData.name)
        val iconToUse = if (bookmarkData.bookmark.daySensitive) {
          BrowserIcons.icon("/icons/10x10_calendar.png")
        } else {
          BrowserIcons.Blank10
        }
        l.setIcon(iconToUse)
        l.setBorder(emptyBorder)
        l
      }
    }
    renderer = ListView.Renderer.wrap(bookmarkDataListCellRenderer)
  }

  val dayPicker = new MinimalSXMonthView {
    border = LineBorder(GuiUtils.BorderColour)
    enabled = valuationDayShouldBeEnabled

    val c = Calendar.getInstance()
    val now = new java.util.Date()
    c.setTime(now)
    val dayOfWeek = c.get(Calendar.DAY_OF_WEEK)
    c.add(Calendar.DAY_OF_WEEK, -1)
    while (c.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY || c.get(Calendar.DAY_OF_WEEK) == Calendar.SATURDAY) {
      c.add(Calendar.DAY_OF_WEEK, -1)
    }
    date = c.getTime

    traversable = true
  }

  def updateSelectedBookmark(b:Bookmark) {
    val data = bookmarksListView.listData
    val indexToSelect = data.zipWithIndex.find{case (bd, _) => bd.bookmark == b} match {
      case None => -1
      case Some((_, index)) => index
    }
    if (indexToSelect != -1) {
      bookmarksListView.selectIndices(indexToSelect)
    }
  }

  def deleteBookmark() {
    val bookmarkSelected = bookmarksListView.selected
    context.submitYesNo("Delete Bookmark?",
      "Are you sure you want to delete the \"" + bookmarkSelected.name + "\" bookmark?",
      DeleteBookmarkRequest(bookmarkSelected.name), (u:Unit) => {false}, (u:Unit) => {})
  }

  def goToBookmark(modifiers:Modifiers) {
    val bookmark = bookmarksListView.selected
    val baseDay = if (bookmark.bookmark.daySensitive) {
      Some(dayPicker.date)
    } else {
      None
    }
    publish(GoingToBookmark)
    context.createAndGoTo(serverContext => {
      val browserDay = baseDay.map( fromJavaDate )
      bookmark.bookmark.createPage(browserDay, serverContext, context)
    }, modifiers = modifiers)
  }

  def fromJavaDate(date:java.util.Date):BrowserDay = {
    val calendar = new GregorianCalendar
    calendar.setTimeInMillis(date.getTime)
    BrowserDay(
      calendar.get(Calendar.YEAR),
      calendar.get(Calendar.MONTH)+ 1,
      calendar.get(Calendar.DAY_OF_MONTH)
    )
  }

  val goToBookmarkAction1 = new AbstractAction("Go", NewPageButton.arrowImage) {
    putValue(javax.swing.Action.SHORT_DESCRIPTION, "Go to the selected bookmark (F9)")
    putValue(javax.swing.Action.MNEMONIC_KEY, swing.event.Key.G.id)
    def actionPerformed(e:ActionEvent) {
      goToBookmark(Modifiers.modifiers(e.getModifiers))
    }
  }
  
  val goToBookmarkButton = new NewPageButton {
    peer.setAction(goToBookmarkAction1)
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
    goToBookmarkAction1.setEnabled(b)
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
    case BookmarksUpdate(username, _) if username == context.localCache.currentUserName => {
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
    case MouseClicked(`bookmarksListView`,_,m,2,_) => goToBookmark(Modifiers.modifiersEX(m))
    case KeyPressed(`bookmarksListView`, scala.swing.event.Key.Delete, _, _) => deleteBookmark()
    case SelectionChanged(`bookmarksListView`) => dayPicker.enabled = valuationDayShouldBeEnabled
    case KeyPressed(`bookmarksListView`, scala.swing.event.Key.Enter, m,_) => goToBookmark(Modifiers.modifiersEX(m))
    case KeyPressed(`dayPicker`, scala.swing.event.Key.Enter, m,_) => goToBookmark(Modifiers.modifiersEX(m))
    case MouseClicked(`dayPicker`,_,m,2,_) => goToBookmark(Modifiers.modifiersEX(m))
  }
  listenTo(context.remotePublisher, bookmarksListView.keys, bookmarksListView.mouse.clicks, bookmarksListView.selection, dayPicker.keys, dayPicker.mouse.clicks)

  if (!bookmarks.isEmpty) {
    bookmarksListView.selectIndices(0)
  }

  componentsEnabled = bookmarks.nonEmpty
}

case object GoingToBookmark extends Event