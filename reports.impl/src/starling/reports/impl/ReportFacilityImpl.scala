package starling.reports.impl

import pivot.{CurveIdentifierFactory, CurveIdentifier, ReportServiceInternal}
import starling.gui.api.ReportParameters._
import starling.pivot.model.{PivotTableModel, DiffPivotTableDataSource}
import starling.gui.api._
import starling.daterange.{Timestamp, Day}
import starling.tradestore.{TradeStores, TradeSet}
import starling.auth.User
import starling.instrument.TradeID._
import starling.instrument.{TradeSystem, TradeID}
import starling.pivot.{FieldDetailsGroup, TableCell, PivotFieldParams}
import starling.rmi.UserSettingsDatabase
import starling.db.{DB, TradeSystems}
import starling.reports.facility.ReportFacility

//        reportContextBuilder:ReportContextBuilder,
//        reportService:ReportFacility,
//        userReportsService:UserReportsService,

class ReportFacilityImpl(
                         reportService:ReportServiceInternal,
                         userReportsService:UserReportsService,
                         tradeStores:TradeStores,
                         userSettingsDatabase:UserSettingsDatabase,
                         eaiStarlingDB:DB,
                         curveIdentifierFactory: CurveIdentifierFactory) extends ReportFacility {

  private def unLabel(tradeID:TradeIDLabel):TradeID = TradeID(tradeID.id, unLabel(tradeID.tradeSystem))
  private def unLabel(tradeSystem:TradeSystemLabel):TradeSystem = TradeSystems.fromName(tradeSystem.name)

  private def label(tradeSystem:TradeSystem) = TradeSystemLabel(tradeSystem.name, tradeSystem.shortCode)
  private def label(fieldDetailsGroup:FieldDetailsGroup):FieldDetailsGroupLabel = FieldDetailsGroupLabel(fieldDetailsGroup.name, fieldDetailsGroup.fields.map(_.field.name))

  def diffReportPivot(tradeSelection:TradeSelection, curveIdentifierDm1:CurveIdentifierLabel, curveIdentifierD:CurveIdentifierLabel,
                      reportOptions:ReportOptions, expiryDay:Day,fromTimestamp:TradeTimestamp, toTimestamp:TradeTimestamp,
                      pivotFieldParams:PivotFieldParams) = {
    val reportDataDMinus1 = reportService.reportPivotTableDataSource(ReportParameters(tradeSelection.withDeskTimestamp(fromTimestamp), curveIdentifierDm1, reportOptions, expiryDay))
    val reportDataD = reportService.reportPivotTableDataSource(ReportParameters(tradeSelection.withDeskTimestamp(toTimestamp), curveIdentifierD, reportOptions, expiryDay))
    val pivot = new DiffPivotTableDataSource(reportDataD._2, reportDataDMinus1._2, "D-1") {
    }
    PivotTableModel.createPivotData(pivot, pivotFieldParams)
  }

  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay: Day, pivotFieldParams: PivotFieldParams) = {
    assert(tradeSelection.intradaySubgroupAndTimestamp.isEmpty, "Can't do a pnl reconciliation with intraday trades")

    val tradeSets: List[(TradeSet, Timestamp)] = tradeStores.toTradeSets(tradeSelection)
    assert(tradeSets.size == 1, "Must have only 1 trade set")
    val tradeSet = tradeSets.head

    val pivot = reportService.pnlReconciliation(curveIdentifierFactory.unLabel(curveIdentifier), tradeSet._1, tradeSet._2, eaiStarlingDB)
    PivotTableModel.createPivotData(pivot, pivotFieldParams)
  }

  def reportErrors(reportParameters:ReportParameters):ReportErrors = reportService.reportErrors(reportParameters)

  def createUserReport(reportParameters: ReportParameters) = userReportsService.createUserReport(reportParameters)
  def createReportParameters(userReportData: UserReportData, observationDay: Day) = userReportsService.createReportParameters(userReportData, observationDay)

  def reportPivot(reportParameters: ReportParameters, pivotFieldParams:PivotFieldParams) = reportService.reportPivot(reportParameters, pivotFieldParams)

  val reportOptionsAvailable = reportService.pivotReportRunner.reportOptionsAvailable


  def tradeValuation(tradeIDLabel:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp, reportSpecificChoices : ReportSpecificChoices):TradeValuationAndDetails = {
    val tradeID = unLabel(tradeIDLabel)
    val stores = tradeStores.storesFor(tradeID.tradeSystem)
    stores.foreach { tradeStore => {
      tradeStore.readTrade(tradeID, Some(timestamp)) match {
        case None =>
        case Some(trade) => {
          val tradeValuation = reportService.singleTradeReport(trade, curveIdentifierFactory.unLabel(curveIdentifier), reportSpecificChoices)

          val (stable, fieldDetailsGroups, _) = tradeStores.readTradeVersions(tradeID)
          val cols = stable.columns

          val tableRow = stable.data.find(row => {
            (row(1).asInstanceOf[TableCell].value == timestamp)
          }).getOrElse(stable.data.last)

          return TradeValuationAndDetails(tradeValuation, tableRow, fieldDetailsGroups, cols)
        }
      }
    }}
    throw new Exception(tradeID + " not found")
  }

  def clearCache {
    reportService.clearCache()
  }

  def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean) {
    userSettingsDatabase.saveUserReport(User.currentlyLoggedOn, reportName, data, showParameters)
  }
  def deleteUserReport(reportName:String) {userSettingsDatabase.deleteUserReport(User.currentlyLoggedOn, reportName)}

  def allUserReports = userReportsService.allUserReports

  def runNamedReport(user: User, reportName: String, day: Day, layout: Option[String]) =
    userReportsService.runNamedReport(user, reportName, day, layout)
}