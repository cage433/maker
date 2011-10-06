package starling.reports.impl

import pivot.ReportServiceInternal
import starling.calendar.BusinessCalendarSet
import starling.tradestore.TradeStores
import starling.db.MarketDataStore
import starling.rmi.UserSettingsDatabase
import starling.auth.User
import starling.pivot.controller.PivotTable
import starling.gui.api._
import starling.daterange.{Timestamp, Day}
import starling.reports.impl.pivot.ReportServiceInternal
import starling.pivot.{PivotFieldParams, PivotFieldsState}

class UserReportsService(
  val ukHolidayCalendar: BusinessCalendarSet,
  tradeStores:TradeStores,
  marketDataStore:MarketDataStore,
  userSettingsDatabase:UserSettingsDatabase,
  reportService:ReportServiceInternal) {

  def extraLayouts(user:User) = userSettingsDatabase.readPivotLayouts(user)
  def allUserReports = userSettingsDatabase.allUserReports

  def pivotTableFor(user:User, reportName:String, day:Day, pivotFieldState:PivotFieldsState): PivotTable = {
    runNamedReportForLayout(user, reportName, day, pivotFieldState)
      .getOrElse(throw new Exception("No report found for " + user.name + " " + reportName))
  }

  def runNamedReportForLayout(user:User, reportName:String, day:Day, pivotFieldState:PivotFieldsState) = {
    withReportParameters(user, reportName, day, (report,reportParameters) => {
      val pivotData = reportService.reportPivot(reportParameters, PivotFieldParams(true, Some(pivotFieldState)))
      pivotData.pivotTable
    })
  }

  def runNamedReport(user:User, reportName:String, day:Day, layout:Option[String]):Option[PivotTable] = {
    withReportParameters(user, reportName, day, (report,reportParameters) => {
        val layouts = userSettingsDatabase.readPivotLayouts(user)
        val layoutName = layout.getOrElse(reportName)
        val reportLayout = layouts.find(_.layoutName == layoutName) match {
          case None => throw new Exception("The layout wasn't found. Please specify a valid layout")
          case Some(rl) => rl
        }
        val pivotData = reportService.reportPivot(reportParameters, PivotFieldParams(true, Some(reportLayout.pivotFieldState)))
        pivotData.pivotTable
      })
  }

  private def withReportParameters[R](user:User, reportName:String, day:Day, action:(UserReport, ReportParameters)=>R) = {
    userSettingsDatabase.findReport(user, reportName) match {
      case Some(report) => {
        val reportParameters = createReportParameters(report.data, day)
        Some(action(report, reportParameters))
      }
      case None => None
    }
  }

  def latestMarketDataIdentifier(selection:MarketDataSelection) = marketDataStore.latestMarketDataIdentifier(selection)

  def createReportParameters(userReportData:UserReportData, baseDay:Day) = {
    val tradeSelection = userReportData.tradeSelection
    val desk = tradeSelection.desk
    val intradaySubgroup = tradeSelection.intradaySubgroup

    val marketDataVersion = marketDataStore.latest(userReportData.marketDataSelection)

    val curveIdentifierLabel = CurveIdentifierLabel(
      MarketDataIdentifier(userReportData.marketDataSelection, marketDataVersion),
      userReportData.environmentRule,
      baseDay,
      applyOffset(baseDay, userReportData.valuationDayOffset).atTimeOfDay(userReportData.valuationDayTimeOfDay),
      applyOffset(baseDay, userReportData.thetaDayOffset).atTimeOfDay(userReportData.thetaDayTimeOfDay),
      userReportData.environmentModifiers
    )

    val pnlOptions = userReportData.pnl.map {
      case (marketDayOffset, bookCloseOffset, useExcel, timeOfDay) => {
        val pnlFromDay = applyOffset(baseDay, marketDayOffset)

        val fromCurveIdentifierLabel = {
          val marketDataSelection = {
            if (useExcel) {
              userReportData.marketDataSelection
            } else {
              userReportData.marketDataSelection.copy(excel = None)
            }
          }
          val marketDataVersion = marketDataStore.latest(marketDataSelection)

          val rule = if(useExcel) {
            EnvironmentRuleLabel.RealTime
          } else {
            EnvironmentRuleLabel.COB
          }
          CurveIdentifierLabel(
            MarketDataIdentifier(marketDataSelection, marketDataVersion),
            rule,
            pnlFromDay,
            pnlFromDay.atTimeOfDay(timeOfDay),
            pnlFromDay.nextBusinessDay(ukHolidayCalendar).atTimeOfDay(userReportData.thetaDayTimeOfDay),
            userReportData.environmentModifiers
          )
        }

        val tradeTS = bookCloseOffset match {
          case Left(offset) => createTradeTimestamp(desk, applyOffset(baseDay, offset))
          case Right(true) if desk.isDefined => tradeStores.closedDesks.latestTradeTimestamp(desk.get)
          case Right(false) if desk.isDefined => tradeStores.titanCurrentTimestamp
          case _ => createTradeTimestamp(desk, baseDay) // this doesn't matter as it'll never be used
        }

        PnlFromParameters(
          Some(tradeTS),
          fromCurveIdentifierLabel
        )
      }
    }

    val bookCloseDay0 = bookCloseDay(desk, userReportData.tradeVersionOffSetOrLive, baseDay)
    val tradeSelectionWithTimestamp = new TradeSelectionWithTimestamp(desk.map((_, bookCloseDay0.get)),
      tradeSelection.tradePredicate, intradaySubgroupAndTimestamp(intradaySubgroup))

    val tradeExpiryDay0 = tradeExpiryDay(baseDay, userReportData.liveOnOffSet)
    ReportParameters(tradeSelectionWithTimestamp, curveIdentifierLabel, userReportData.reportOptions, tradeExpiryDay0, pnlOptions)
  }

  def createUserReport(reportParameters:ReportParameters):UserReportData = {
    val baseDay = reportParameters.curveIdentifier.tradesUpToDay
    val bookCloseOffset = tradeVersionOffsetOrLatest(baseDay, reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp)
    UserReportData(
      tradeSelection = reportParameters.tradeSelectionWithTimestamp.asTradeSelection,
      marketDataSelection = reportParameters.curveIdentifier.marketDataIdentifier.selection,
      environmentModifiers = reportParameters.curveIdentifier.envModifiers,
      reportOptions = reportParameters.reportOptions,
      environmentRule = reportParameters.curveIdentifier.environmentRule,
      valuationDayOffset = businessDaysBetween(baseDay, reportParameters.curveIdentifier.valuationDayAndTime.day),
      valuationDayTimeOfDay = reportParameters.curveIdentifier.valuationDayAndTime.timeOfDay,
      thetaDayOffset = businessDaysBetween(baseDay, reportParameters.curveIdentifier.thetaDayAndTime.day),
      thetaDayTimeOfDay = reportParameters.curveIdentifier.thetaDayAndTime.timeOfDay,
      tradeVersionOffSetOrLive = bookCloseOffset,
      liveOnOffSet = liveOnOffsetOrStartOfYear(baseDay, reportParameters.expiryDay),
      pnl = reportParameters.pnlParameters.map {
        case pnl => {
          val marketDayOffset = businessDaysBetween(baseDay, pnl.curveIdentifierFrom.tradesUpToDay)
          val timeOfDay = pnl.curveIdentifierFrom.valuationDayAndTime.timeOfDay
          val bookCloseOffset = pnl.tradeTimestampFrom match {
            case Some(ts) => {
              reportParameters.tradeSelectionWithTimestamp.deskAndTimestamp match {
                case Some((d,_)) => tradeVersionOffsetOrLatest(baseDay, d,  ts)
                case None => Left(0)
              }
            }
            case None => Left(0)
          }
          (marketDayOffset, bookCloseOffset, pnl.curveIdentifierFrom.marketDataIdentifier.selection.excel != None, timeOfDay)
        }
      }
    )
  }

  private def businessDaysBetween(day1:Day, day2:Day) = {
    if (!ukHolidayCalendar.isBusinessDay(day1) || !ukHolidayCalendar.isBusinessDay(day2) ) {
      0 //A hack just to stop exceptions if ever run on a holiday. I don't know what the correct behaviour is
    } else {
      day1.businessDaysBetween(day2, ukHolidayCalendar)
    }
  }

  private def tradeVersionOffsetOrLatest(baseDay : Day, d : Desk, ts : TradeTimestamp) : Either[Int, Boolean] = {
    val latestClose = tradeStores.closedDesks.latestTradeTimestamp(d)
    if (ts.closeDay == latestClose.closeDay) {
      Right(true)
    } else if (ts.closeDay == TradeTimestamp.magicLatestTimestampDay) {
      Right(false)
    } else {
      Left(businessDaysBetween(baseDay, ts.closeDay))
    }
  }

  def tradeVersionOffsetOrLatest(baseDay:Day, deskAndTimestamp:Option[(Desk, TradeTimestamp)]) : Either[Int, Boolean] = {
    deskAndTimestamp match {
      case Some((d,ts)) => tradeVersionOffsetOrLatest(baseDay, d, ts)
      case None => Left(0)
    }
  }

  def liveOnOffsetOrStartOfYear(baseDay:Day, expiryDay:Day) = {
    if (baseDay.startOfFinancialYear == expiryDay) {
      Right(true)
    } else {
      Left(businessDaysBetween(baseDay, expiryDay))
    }
  }

  def createTradeTimestamp(desk:Option[Desk], closeDay:Day) = {
    desk match {
      case Some(d) => tradeStores.deskDefinitions(d).tradeTimestampForOffset(closeDay)
      case None => throw new Exception("No desk")
    }
  }

  def bookCloseDay(desk:Option[Desk], tradeVersionOffSetOrLive:Either[Int,Boolean], baseDay:Day) = {
    tradeVersionOffSetOrLive match {
      case Left(offset) if desk.isDefined => Some(createTradeTimestamp(desk, applyOffset(baseDay, offset)))
      case Right(_) if desk.isDefined => Some(tradeStores.closedDesks.latestTradeTimestamp(desk.get))
      case _ => None
    }
  }

  def applyOffset(base:Day,numberOfDays:Int) = {
    base.addBusinessDays(ukHolidayCalendar, numberOfDays)
  }

  def intradaySubgroupAndTimestamp(intradaySubgroup:Option[IntradayGroups]):Option[(IntradayGroups, Timestamp)] = {
    intradaySubgroup.map(intra => {
      val latestIntradayTimestamp = {
        val groupsToUserTimestamp = tradeStores.intradayTradeStore.intradayLatest
        val validGroups = groupsToUserTimestamp.keySet.filter(g => {
          intra.subgroups.toSet.exists(t => g.startsWith(t))
        })
        validGroups.map(g => groupsToUserTimestamp(g)._2).max
      }
      (intra, latestIntradayTimestamp)
    })
  }

  def tradeExpiryDay(baseDay:Day, either:Either[Int,Boolean]):Day = {
    either match {
      case Left(offset) => applyOffset(baseDay, offset)
      case Right(_) => baseDay.startOfFinancialYear
    }
  }
}
