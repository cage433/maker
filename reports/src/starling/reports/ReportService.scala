package starling.reports

import starling.daterange.{Timestamp, Day}
import starling.gui.api._
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.auth.User
import starling.pivot.controller.PivotTable
import starling.manager.DoNotCache

trait ReportService {

  def reportOptionsAvailable:ReportOptionsAvailable
  def reportPivot(reportParameters:ReportParameters, pivotFieldParams:PivotFieldParams):PivotData
  def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel, reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp, pivotFieldParams:PivotFieldParams):PivotData
  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay:Day, pivotFieldParams: PivotFieldParams): PivotData
  def reportErrors(reportParameters:ReportParameters):ReportErrors
  def tradeValuation(tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp):TradeValuationAndDetails
  @DoNotCache def createReportParameters(userReportData:UserReportData, observationDay:Day):ReportParameters
  @DoNotCache def createUserReport(reportParameters:ReportParameters):UserReportData
  @DoNotCache def deleteUserReport(reportName:String)
  @DoNotCache def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean)
  @DoNotCache def clearCache:Unit


  /*for regression runner */ @DoNotCache def allUserReports:Map[String,List[UserReport]]
  /*for regression runner */ @DoNotCache def runNamedReport(user:User, reportName:String, day:Day, layout:Option[String]):Option[PivotTable]
}