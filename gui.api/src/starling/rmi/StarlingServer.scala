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

case class StarlingServerInitialData(
  name:String,
  version:Version,
  ukBusinessCalendar:BusinessCalendar,
  whoAmI:User,
  isStarlingDeveloper:Boolean,
  allUserNames:List[String]
)

trait StarlingServer {
  def name:String
  @DoNotCache def init():StarlingServerInitialData
  def referenceDataTables():List[ReferenceDataLabel]
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData

  @DoNotCache def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData
  @DoNotCache def storeSystemInfo(info:OSInfo)
  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData
}