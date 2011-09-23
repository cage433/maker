package starling.rmi

import starling.gui.api._
import starling.services.{ForwardCurveCommitResponse, ForwardCurveInfo}
import starling.pivot._
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.eai.{Traders}
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.browser.service.{PageLogInfo, BookmarkLabel, UserSettingsLabel, Version}
import starling.utils.{CaseInsensitive, STable}
import starling.manager.DoNotCache

/**
 * StarlingServer defines the contract for the remote interface to the starling server.  Its 12 methods provide access
 * to the reference, various types of pivot report and other types data, such as a list of traders, desks and business
 * calendars.
 * 
 * @documented
 */
trait StarlingServer {
  /**@return This server's name   */
  def name:String
  /**@return This server's version. */
  def version:Version
  /**@return The labels wrapping this server's reference data tables' names.*/
  def referenceDataTables():List[ReferenceDataLabel]
  /**@return The reference data pivot data for the given parameters. */
  def referencePivot(table:ReferenceDataLabel, pivotFieldParams:PivotFieldParams):PivotData
  /**@return The UK business calendar. */
  def ukBusinessCalendar:BusinessCalendar
  /**@return The currently logged-on user.*/
  def whoAmI:User
  /**@return The organisation pivot data for the given field parameters.*/
  @DoNotCache def orgPivot(pivotFieldParams:PivotFieldParams):PivotData
  /**@return The name of each user. */
  @DoNotCache def allUserNames:List[String]
  def isStarlingDeveloper:Boolean
  /**@return The user pivot data for the given parameters. */
  @DoNotCache def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData
  /**Persists the given operating system information for the currently logged-on user.*/
  @DoNotCache def storeSystemInfo(info:OSInfo)
  /**
   * @param pivotFieldParams The pivot fields.
   * @param numCommits The number of commits back from the most recent to include.
   * @return The pivot retrieved from GIT for the given parameters.
   */
  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int):PivotData
}
