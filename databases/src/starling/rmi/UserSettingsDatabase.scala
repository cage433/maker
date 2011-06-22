package starling.rmi

import starling.utils.sql.LiteralString
import starling.db.DB
import starling.gui.UserSettings
import starling.utils.sql.QueryBuilder._
import starling.auth.User
import starling.gui.api._
import com.thoughtworks.xstream.io.StreamException
import starling.utils.{StarlingXStream, Broadcaster}
import starling.pivot._
import collection.mutable.ListBuffer
import starling.daterange.{Day, Timestamp}
import starling.calendar.BusinessCalendar

case class DayOffSet(offset:Int)

class UserSettingsDatabase(val db:DB, broadcaster:Broadcaster) {
  // This is the standard number of characters in a var char column.
  private val colSize = 300

  def loadSettings:UserSettings = {
    val user = User.currentlyLoggedOn.username.take(colSize)
    val q = select ("settings") from "usersettings" where ("starlinguser" eql LiteralString(user))
    db.queryWithOneResult(q) { rs => rs.getString("settings")} match {
      case Some(xml : String) => try {
        StarlingXStream.read(xml).asInstanceOf[UserSettings]
      } catch {
        case e:StreamException => new UserSettings //If the UserSettings classes have changed just return empty settings
      }
      case None => new UserSettings
    }
  }

  def readAll() {
    allSettings
    allPivotLayouts
    allBookmarks
  }

  def allSettings:List[UserSettings] = {
    db.queryWithResult("SELECT * from usersettings") {
      rs => {
        StarlingXStream.read(rs.getString("settings")).asInstanceOf[UserSettings]
      }
    }
  }

  def saveSettings(settings: UserSettings) {
    val user = User.currentlyLoggedOn.username.take(colSize)
    db.inTransaction {
      writer => {
        writer.delete("usersettings", ("starlinguser" eql LiteralString(user)))
        writer.insert("usersettings", Map("starlinguser" -> user, "settings" -> StarlingXStream.write(settings)))
      }
    }
  }
  
  def savePivotLayout(user:User, pivotLayout:PivotLayout) {
    val username = user.username.take(colSize)
    val layoutName = pivotLayout.layoutName.take(colSize)
    db.inTransaction {
      writer => {
        val associatedReports:String = {
          if (pivotLayout.associatedReports.nonEmpty) {
            pivotLayout.associatedReports.mkString(PivotLayout.AssociatedReportsDelimiter)
          } else null
        }
        writer.insert("PivotLayouts", Map("starlingUser" -> username, "layoutName" -> layoutName,
          "layout" -> StarlingXStream.write(replaceDayWithOffset(pivotLayout.pivotFieldState)),
          "otherLayoutInfo" -> StarlingXStream.write(pivotLayout.otherLayoutInfo),
          "layoutType" -> pivotLayout.layoutType, "associatedReport" -> associatedReports))
      }
    }
    broadcaster.broadcast(PivotLayoutUpdate(user.username, readPivotLayouts(user)))
  }

  private def replaceDayWithOffset(pfs:PivotFieldsState) = {
    pfs.mapSelectionValues( v => {
      v match {
        case d:Day => {
          val offset = Day.today.businessDaysBetween(d, BusinessCalendar.NONE)
          if (offset <=0 && offset >= -2) DayOffSet(offset) else d
        }
        case other => other
      }
    })
  }
  private def replaceOffsetWithDay(pfs:PivotFieldsState) = {
    pfs.mapSelectionValues( v => {
      v match {
        case DayOffSet(offset) => Day.today.addBusinessDays(BusinessCalendar.NONE, offset)
        case other => other
      }
    })
  }
  def readPivotLayouts(user:User):List[PivotLayout] = {
    val username = user.username.take(colSize)
    db.queryWithResult("SELECT * from PivotLayouts where starlingUser = :user", Map("user" -> username)) {
      rs => {
        val associatedReports = {
          val ar = rs.getString("associatedReport")
          if (ar != null) {
            ar.split(PivotLayout.AssociatedReportsDelimiter).toList
          } else {
            List()
          }
        }
        PivotLayout(rs.getString("layoutName"),
          replaceOffsetWithDay(StarlingXStream.read(rs.getString("layout")).asInstanceOf[PivotFieldsState]),
          true, StarlingXStream.read(rs.getString("otherLayoutInfo")).asInstanceOf[OtherLayoutInfo],
          rs.getString("layoutType"), associatedReports)
      }
    }
  }

  def allPivotLayouts:List[PivotLayout] = {
    db.queryWithResult("SELECT * from PivotLayouts") {
      rs => {
        val associatedReports = {
          val ar = rs.getString("associatedReport")
          if (ar != null) {
            ar.split(PivotLayout.AssociatedReportsDelimiter).toList
          } else {
            List()
          }
        }
        PivotLayout(rs.getString("layoutName"), StarlingXStream.read(rs.getString("layout")).asInstanceOf[PivotFieldsState],
          true, StarlingXStream.read(rs.getString("otherLayoutInfo")).asInstanceOf[OtherLayoutInfo],
          rs.getString("layoutType"), associatedReports)
      }
    }
  }

  def deletePivotLayout(user:User, layoutName:String) {
    val username = user.username.take(colSize)
    db.inTransaction {
      writer => {
        writer.delete("PivotLayouts", ("starlingUser" eql LiteralString(username)) and ("layoutName" eql layoutName))
      }
    }
    broadcaster.broadcast(PivotLayoutUpdate(user.username, readPivotLayouts(user)))
  }

  def userReports(user:User) = {
    val userName = user.username.take(colSize)
    db.queryWithResult("SELECT * FROM UserReports where starlingUser = :user", Map("user" -> userName)) {
      rs => {
        UserReport(rs.getString("reportName"), StarlingXStream.read(rs.getString("report")).asInstanceOf[UserReportData],
          rs.getBoolean("showParameters"))
      }
    }
  }

  def findReport(user:User, reportName:String) = {
    val userReportPairs = allUserReports.toList.flatMap{ case(username, reports) => reports.map(r=>(username, r)) }
    userReportPairs.find { case(un, report) => un==user.username && report.reportName==reportName }.map(_._2)
  }

  def allUserReports:Map[String,List[UserReport]] = {
    db.queryWithResult("SELECT * FROM UserReports", Map()) {
      rs => {
        val user = rs.getString("starlingUser")
        val report = UserReport(rs.getString("reportName"), StarlingXStream.read(rs.getString("report")).asInstanceOf[UserReportData],
          rs.getBoolean("showParameters"))
        (user, report)
      }
    }.groupBy(_._1).mapValues(_.map(_._2))
  }

  def bookmarks(user:User):List[BookmarkLabel] = {
    val userName = user.username.take(colSize)
    db.queryWithResult("SELECT * FROM Bookmarks where starlingUser = :user", Map("user" -> userName)) {
      rs => BookmarkLabel(rs.getString("bookmarkName"), rs.getString("bookmark"))
    }
  }

  def allBookmarks:Map[String, List[BookmarkLabel]] = {
    db.queryWithResult("SELECT * FROM Bookmarks", Map()) {
      rs => {
        val user = rs.getString("starlingUser")
        val bookmark = BookmarkLabel(rs.getString("bookmarkName"), rs.getString("bookmark"))
        (user, bookmark)
      }
    }.groupBy(_._1).mapValues(_.map(_._2))
  }

  def saveBookmark(user:User, bookmarkLabel:BookmarkLabel) {
    val username = user.username.take(colSize)
    val bookmarkName = bookmarkLabel.name.take(colSize)
    db.inTransaction {
      writer => {
        writer.insert("Bookmarks", Map("starlingUser" -> username, "bookmarkName" -> bookmarkName, "bookmark" -> bookmarkLabel.bookmark))
      }
    }
    broadcaster.broadcast(BookmarksUpdate(user.username, bookmarks(user)))
  }

  def deleteBookmark(user:User, bookmarkName:String) {
    val userName = user.username.take(colSize)
    db.inTransaction {
      writer => {
        writer.delete("Bookmarks", ("starlingUser" eql LiteralString(userName)) and ("bookmarkName" eql LiteralString(bookmarkName)))
      }
    }
    broadcaster.broadcast(BookmarksUpdate(user.username, bookmarks(user)))
  }

  def logPageView(info:PageLogInfo) {
    val user = User.currentlyLoggedOn
    val userName = user.username.take(colSize)
    val actualUser = user.name.take(colSize)

    val iText = info.text.take(1000)
    val iShortText = info.shortText.take(colSize)
    val iPageString = info.pageString.take(8000)
    db.inTransaction {
      writer => {
        writer.insert("PageViewLog", Map("loginName" -> userName, "userName" -> actualUser, "text" -> iText,
          "shortText" -> iShortText, "pageString" -> iPageString, "time" -> info.time))
      }
    }
  }

  def allUsersWithSettings = {
    db.queryWithResult("SELECT starlinguser from usersettings") {
      rs => {
        rs.getString("starlinguser")
      }
    }.sorted
  }

  def storeSystemInfo(user:User, info:OSInfo) {
    val userName = user.username.take(colSize)
    val timestamp = new Timestamp
    db.inTransaction {
      writer => {
        writer.insert("UserSystemInfo", Map("starlingUser" -> userName, "logOnTime" -> timestamp, "info" -> info.toString))
      }
    }
  }

  def deleteUserReport(user:User, reportName:String) {
    val userName = user.username.take(colSize)
    db.inTransaction {
      writer => {
        writer.delete("UserReports", ("starlingUser" eql LiteralString(userName)) and ("reportName" eql LiteralString(reportName)))
      }
    }
  }

  def saveUserReport(user:User, reportName0:String, data:UserReportData, showParameters:Boolean) {
    val username = user.username.take(colSize)
    val reportName = reportName0.take(colSize)
    db.inTransaction {
      writer => {
        writer.insert("UserReports", Map("starlingUser" -> username, "reportName" -> reportName,
          "report" -> StarlingXStream.write(data), "showParameters" -> showParameters))
      }
    }
  }
}