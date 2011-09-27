package starling.rmi

import starling.dbx.QueryBuilder._
import starling.db.DB
import starling.auth.User
import starling.gui.api._
import starling.instrument.utils.StarlingXStream
import starling.utils.{Broadcaster}
import starling.pivot._
import starling.daterange.{Day, Timestamp}
import starling.calendar.BusinessCalendar
import starling.browser.service._
import starling.dbx.{LiteralString, Clause, Query}

case class DayOffSet(offset:Int)

class UserSettingsDatabase(val db:DB, broadcaster:Broadcaster) {
  // This is the standard number of characters in a var char column.
  private val colSize = 300

  def loadSettings(user:User):UserSettingsLabel = {
    val username = user.username.take(colSize)
    val q = select ("bundle, description, settings") from "usersettings" where ("starlinguser" eql LiteralString(username))
    UserSettingsLabel(db.queryWithResult(q) { rs => UserSettingsEntry(
      rs.getString("bundle"), rs.getString("description"), rs.getString("settings")
    )})
  }

  def readAll() {
    allPivotLayouts
  }

  def saveSettings(user:User, settings:UserSettingsLabel) {
    db.inTransaction {
      writer => {
        writer.delete("usersettings", ("starlinguser" eql LiteralString(user.username.take(colSize))))
        settings.userSettings.foreach { entry =>
          writer.insert("usersettings", Map("starlinguser" -> user, "bundle" -> entry.bundle, "description" -> entry.key, "settings" -> entry.value))
        }
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
      rs => BookmarkLabel(rs.getString("bookmarkName"), rs.getString("bundle"), rs.getString("bookmark"))
    }
  }

  def saveBookmark(user:User, bookmarkLabel:BookmarkLabel) {
    val username = user.username.take(colSize)
    val bookmarkName = bookmarkLabel.name.take(colSize)
    val bundle = bookmarkLabel.bundleName
    db.inTransaction {
      writer => {
        writer.insert("Bookmarks", Map("starlingUser" -> username, "bookmarkName" -> bookmarkName,
          "bundle" -> bundle, "bookmark" -> bookmarkLabel.bookmark))
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
    db.queryWithResult("SELECT distinct(starlinguser) from usersettings") {
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