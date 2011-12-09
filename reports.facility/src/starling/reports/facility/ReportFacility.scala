package starling.reports.facility

import starling.daterange.{Timestamp, Day}
import starling.gui.api._
import starling.pivot.PivotFieldParams
import starling.rmi.PivotData
import starling.auth.User
import starling.pivot.controller.PivotTable
import starling.manager.Memoize

trait ReportFacility {

  @Memoize def reportOptionsAvailable:ReportOptionsAvailable
  @Memoize def reportPivot(reportParameters:ReportParameters, pivotFieldParams:PivotFieldParams):PivotData
  @Memoize def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel, reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp, pivotFieldParams:PivotFieldParams):PivotData
  @Memoize def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay:Day, pivotFieldParams: PivotFieldParams): PivotData
  @Memoize def reportErrors(reportParameters:ReportParameters):ReportErrors
  @Memoize def tradeValuation(tradeID:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp, reportSpecificChoices : ReportSpecificChoices):TradeValuationAndDetails
  def createReportParameters(userReportData:UserReportData, observationDay:Day):ReportParameters
  def createUserReport(reportParameters:ReportParameters):UserReportData
  def deleteUserReport(reportName:String)
  def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean)
  def clearCache:Unit


  /*for regression runner */ def allUserReports:Map[String,List[UserReport]]
  /*for regression runner */ def runNamedReport(user:User, reportName:String, day:Day, layout:Option[String]):Option[PivotTable]
}