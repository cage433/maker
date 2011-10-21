package starling.rmi

import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.browser.service.{PageLogInfo, BookmarkLabel, UserSettingsLabel, Version}
import starling.utils.{CaseInsensitive, STable}
import starling.manager.DoNotCache

/**
 * This is the remote interface to the starling server.
 */
trait StarlingServer {
  def name:String
  def version:Version

  def referenceDataTables():List[ReferenceDataLabel]
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData
  def ukBusinessCalendar:BusinessCalendar

  def whoAmI:User
  @DoNotCache def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def allUserNames:List[String]
  def isStarlingDeveloper:Boolean
  @DoNotCache def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def storeSystemInfo(info:OSInfo)
  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData
}