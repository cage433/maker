package starling.rmi

import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.browser.service.{PageLogInfo, BookmarkLabel, UserSettingsLabel, Version}
import starling.utils.{CaseInsensitive, STable}
import starling.manager.Memoize

case class StarlingServerInitialData(
  name:String,
  version:Version,
  ukBusinessCalendar:BusinessCalendar,
  whoAmI:User,
  isStarlingDeveloper:Boolean,
  allUserNames:List[String]
)

trait StarlingServer {
  @Memoize def name:String
  def init():StarlingServerInitialData
  @Memoize def referenceDataTables():List[ReferenceDataLabel]
  @Memoize def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData

  def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData
  def storeSystemInfo(info:OSInfo)
  @Memoize def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData
}