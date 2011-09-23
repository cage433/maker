package starling.reports

import starling.daterange.{Timestamp, Day}
import starling.gui.api._
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.auth.User
import starling.pivot.controller.PivotTable

trait ReportService {

  def reportOptionsAvailable:ReportOptionsAvailable
  def reportPivot(reportParameters:ReportParameters, pivotFieldParams:PivotFieldParams):PivotData
  def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel, reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp, pivotFieldParams:PivotFieldParams):PivotData
  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay:Day, pivotFieldParams: PivotFieldParams): PivotData
  def reportErrors(reportParameters:ReportParameters):ReportErrors
  def tradeValuation(tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp):TradeValuationAndDetails
  def createReportParameters(userReportData:UserReportData, observationDay:Day):ReportParameters
  def createUserReport(reportParameters:ReportParameters):UserReportData
  def deleteUserReport(reportName:String)
  def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean)
  def clearCache:Unit


  /*for regression runner */def allUserReports:Map[String,List[UserReport]]
  /*for regression runner */def runNamedReport(user:User, reportName:String, day:Day, layout:Option[String]):Option[PivotTable]
}