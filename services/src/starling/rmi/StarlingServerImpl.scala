package starling.rmi

import collection.immutable.{Set, List}
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import starling.auth.{LdapUserLookup, User}
import starling.calendar.BusinessCalendarSet
import starling.daterange._
import starling.db._
import starling.eai.{Book, Traders}
import starling.gui.api._
import starling.pivot._
import controller.PivotTable
import starling.pivot.model._
import starling.reports.pivot._
import starling.services._
import starling.trade.{Trade, TradeID, TradeSystem}
import starling.tradestore.{TradeSet, TradePredicate, TradeStores}
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.utils._
import starling.dbx.{FalseClause, From, RealTable}

import starling.dbx.QueryBuilder._
import starling.browser.service.Version

class UserReportsService(
  val ukHolidayCalendar: BusinessCalendarSet,
  tradeStores:TradeStores,
  marketDataStore:MarketDataStore,
  userSettingsDatabase:UserSettingsDatabase,
  reportService:ReportService) {

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
          case Right(_) if desk.isDefined => tradeStores.closedDesks.latestTradeTimestamp(desk.get)
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
                case Some((d,_)) => {
                  val latestClose = tradeStores.closedDesks.latestTradeTimestamp(d)
                  if (ts.closeDay == latestClose.closeDay) {
                    Right(true)
                  } else {
                    Left(businessDaysBetween(baseDay, ts.closeDay))
                  }
                }
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

  def tradeVersionOffsetOrLatest(baseDay:Day, deskAndTimestamp:Option[(Desk, TradeTimestamp)]) = {
    deskAndTimestamp match {
      case Some((d,ts)) => {
        val latestClose = tradeStores.closedDesks.latestTradeTimestamp(d)
        if (ts.closeDay == latestClose.closeDay) {
          Right(true)
        } else {
          Left(businessDaysBetween(baseDay, ts.closeDay))
        }
      }
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

class StarlingServerImpl(
        val name:String,
        reportContextBuilder:ReportContextBuilder,
        reportService:ReportService,
        userSettingsDatabase:UserSettingsDatabase,
        userReportsService:UserReportsService,
        tradeStores:TradeStores,
        enabledDesks: Set[Desk],
        versionInfo:Version,
        referenceData:ReferenceData,
        ukHolidayCalendar: BusinessCalendarSet,
        ldapSearch: LdapUserLookup,
        eaiStarlingDB: DB,
        val allTraders: Traders
      ) extends StarlingServer with Log {

  def desks = {
    val user = User.currentlyLoggedOn
    val enabled = tradeStores.deskDefinitions.keysIterator.toList.filter(enabledDesks.contains)
    val desksAllowed = Permission.desks(user)
    val userDesks = enabled.filter(desksAllowed.contains)
    log.info("Getting desks for user: " + user.name + ", desks: " + userDesks)
    userDesks
  }

  def groupToDesksMap = Permission.groupToDesksMap

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

  def tradeChanges(tradeSelection:TradeSelection, from:Timestamp, to:Timestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams) = {
    val tradeSets = toIntraddayTradeSets(tradeSelection, None) ::: deskTradeSets(tradeSelection.desk, tradeSelection.tradePredicate)
    val pivots = tradeSets.map { tradeSet =>
      reportService.tradeChanges(tradeSet, from, to, expiryDay:Day)
    }
    PivotTableModel.createPivotData(UnionPivotTableDataSource.join(pivots), pivotFieldParams)
  }

  def tradeReconciliation(tradeSelection:TradeSelection, from:TradeTimestamp, to:TradeTimestamp, intradayTimestamp: Timestamp, pivotFieldParams:PivotFieldParams) = {
    val eaiTrades = deskTradeSets(tradeSelection.desk, tradeSelection.tradePredicate)
    assert(eaiTrades.size == 1, "Must specifiy exactly 1 desk selection: " + eaiTrades)
    // we want to reconcile against (start day + 1) up to end day (inclusive). Quite often this will just be one day.
    val entryDays = (from.closeDay upto to.closeDay).toList.filterNot(_ == from.closeDay)
    val intradayTrades = toIntraddayTradeSets(tradeSelection, Some(entryDays))
    val pivot = reportService.tradeReconciliation(eaiTrades.head, from.timestamp, to.timestamp, intradayTimestamp, intradayTrades)
    PivotTableModel.createPivotData(pivot, pivotFieldParams)
  }

  def pnlReconciliation(tradeSelection: TradeSelectionWithTimestamp, curveIdentifier: CurveIdentifierLabel, expiryDay: Day, pivotFieldParams: PivotFieldParams) = {
    assert(tradeSelection.intradaySubgroupAndTimestamp.isEmpty, "Can't do a pnl reconciliation with intraday trades")

    val tradeSets: List[(TradeSet, Timestamp)] = tradeStores.toTradeSets(tradeSelection)
    assert(tradeSets.size == 1, "Must have only 1 trade set")
    val tradeSet = tradeSets.head

    val pivot = reportService.pnlReconciliation(CurveIdentifier.unLabel(curveIdentifier), tradeSet._1, tradeSet._2, eaiStarlingDB)
    PivotTableModel.createPivotData(pivot, pivotFieldParams)
  }

  def reportErrors(reportParameters:ReportParameters):ReportErrors = reportService.reportErrors(reportParameters)
    
  def createUserReport(reportParameters: ReportParameters) = userReportsService.createUserReport(reportParameters)
  def createReportParameters(userReportData: UserReportData, observationDay: Day) = userReportsService.createReportParameters(userReportData, observationDay)

  def reportPivot(reportParameters: ReportParameters, pivotFieldParams:PivotFieldParams) = reportService.reportPivot(reportParameters, pivotFieldParams)

  val reportOptionsAvailable = reportService.pivotReportRunner.reportOptionsAvailable


  def tradeValuation(tradeIDLabel:TradeIDLabel, curveIdentifier:CurveIdentifierLabel, timestamp:Timestamp):TradeValuationAndDetails = {
    val tradeID = unLabel(tradeIDLabel)
    val stores = tradeStores.storesFor(tradeID.tradeSystem)
    stores.foreach { tradeStore => {
      tradeStore.readTrade(tradeID, Some(timestamp)) match {
        case None =>
        case Some(trade) => {
          val tradeValuation = reportService.singleTradeReport(trade, CurveIdentifier.unLabel(curveIdentifier))

          val (stable, fieldDetailsGroups, _) = readTradeVersions(tradeIDLabel)
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


//  def latestTradeTimestamp(desk:Option[Desk], excel:Option[String]) = {
//    val tradeSets = deskTradeSets(desk, TradePredicate.Null)
//    val deskTimestamps = tradeSets.map(tradeStore => tradeStore.latestTimestamp())
//    val allTimestamps = deskTimestamps ::: excel.map(g=>tradeStores.intradayTradeStore.latestTimestamp()).toList
//    allTimestamps match {
//      case Nil => Timestamp(0)
//      case list => list.max
//    }
//  }

  /**
   * Desk to TradeSet with no filters.
   */
  private def deskTradeSets(desk:Desk):List[TradeSet] = deskTradeSets(Some(desk), TradePredicate(List(), List()))
  private def deskTradeSets(desk:Option[Desk], tradePredicate:TradePredicate):List[TradeSet] = {
    desk.toList.flatMap(desk=>tradeStores.deskDefinitions(desk).tradeSets(tradePredicate))
  }

  /**
   * return a tradeset for an intraday trade selection, including the trade predicate.
   * If entrayDays is specified then amend the predicate to only match trades with the same entry days.
   */
  private def toIntraddayTradeSets(tradeSelection:TradeSelection, entryDays: Option[List[Day]]):List[TradeSet] = {
    val currentGroups = tradeStores.intradayTradeStore.intradayLatest.keySet
    tradeSelection.intradaySubgroup.toList.flatMap {
      subgroups => {
        val subgroupsToUse = subgroups.subgroups.flatMap(subgroup => {
          if (currentGroups.contains(subgroup)) {
            List(subgroup)
          } else {
            currentGroups.filter(_.startsWith(subgroup + "/")).toList
          }
        })
        val predicate = (Field(IntradayTradeAttributes.subgroupName_str), SomeSelection(Set(subgroupsToUse.toSet))) ::
                entryDays.toList.map(d => (Field("Entry Date"), new SomeSelection(d.toSet)))
        List(new TradeSet(IntradayTradeSystem, tradeStores.intradayTradeStore, None,
          TradePredicate(predicate ::: tradeSelection.tradePredicate.filter, tradeSelection.tradePredicate.selection)))
      }
    }.toList
  }

  def tradePivot(tradeSelection: TradeSelectionWithTimestamp, expiryDay:Day, pivotFieldParams:PivotFieldParams) = {
    val pivots = tradeStores.toTradeSets(tradeSelection).map { case (tradeSet, ts) => tradeSet.pivot(expiryDay, ts) }.toList
    val pivot = pivots.size match {
      case 0 => NullPivotTableDataSource
      case _ => UnionPivotTableDataSource.join(pivots)
    }
    val pivotFieldState = pivotFieldParams.pivotFieldState
    def initialPivotState = {
      tradeSelection.deskAndTimestamp match {
        case Some((desk, timestamp)) => {
          tradeStores.deskDefinitions(desk).initialState match {
            case Some(deskDefault) if (validFieldsState(deskDefault)) => deskDefault
            case _ => pivot.initialState
          }
        }
        case None => pivot.initialState
      }
    }
    def validFieldsState(fieldsState:PivotFieldsState) = {
      val fields = Set() ++ pivot.fieldDetails.map(_.field)
      fieldsState.allFieldsUsed.forall(fields.contains(_))
    }
    val fs = pivotFieldState match {
      case Some(f) => {
        if (validFieldsState(f)) {
          f
        } else {
          initialPivotState
        }
      }
      case None => {
        initialPivotState
      }
    }
    val pivotTable = if (pivotFieldParams.calculate) {
      PivotTableModel.createPivotTableData(pivot, fs)
    } else {
      PivotTable.singleCellPivotTable("Calculation is off")
    }
    val fieldGroups = pivot.fieldDetailsGroups.map(_.toFieldGroup)

    val reportSpecificOptions = pivot.reportSpecificOptions
    val fsToUse = PivotTableModel.setDefaultReportSpecificChoices(reportSpecificOptions, fs)

    val pivotData = PivotData(
      pivot.fieldDetails.map(f=>f.field).toList,
      fieldGroups, Set() ++ pivot.fieldDetails.filter(_.isDataField).map(_.field),
      fsToUse,
      pivot.drillDownGroups,
      pivotTable,
      pivot.availablePages,
      reportSpecificOptions)
    tradeSelection.deskAndTimestamp match {
      case Some((d,t)) if t.error != None => {
        val pt = PivotTable.singleCellPivotTable("Book close error")
        pivotData.copy(pivotTable = pt)
      }
      case _ => pivotData
    }
  }

  def readTradeVersions(tradeIDLabel:TradeIDLabel):(STable,List[FieldDetailsGroupLabel],List[CostsLabel]) = {
    val tradeID = unLabel(tradeIDLabel)
    tradeStores.storesFor(tradeID.tradeSystem).foreach { tradeStore => {
      tradeStore.tradeHistory(tradeID) match {
        case Some(res) => return (res._1, res._2.map(label(_)), res._3)
        case None =>
      }
    }}
    throw new Exception("Trade " + tradeIDLabel + " not found")

  }

  private val importTradesMap = new ConcurrentHashMap[Desk,Boolean]

  def importTitanTrades() {
    val ts = Timestamp.now
    val changed = tradeStores.tradeImporters(TitanTradeSystem).importAll(None, ts)
    if (changed) {
      tradeStores.closedDesks.closeDesk(Desk.Titan, Day.today, ts)
    }
  }

  def bookClose(desk: Desk) {
    val bookID = desk match {
      case Desk.GasolineSpec => Book.GasolineSpec.bookID
      case Desk.LondonDerivatives => Book.LondonDerivatives.bookID
      case _ => throw new Exception("Book close is not enabled for " + desk)
    }
    val uuid = UUID.randomUUID.toString
    try {
      eaiStarlingDB.inTransaction {
        writer => {
          writer.update("{call spArchiveTrades(" + bookID + ", '" + uuid + "')}")
        }
      }
    } catch {
      case e => {
        log.error("Error doing book close", e)
      }
    }
  }

  def tradeImportText(tradeSelection:TradeSelection) = {
    tradeSelection.desk match {
      case Some(d) => {
        if (!importTradesMap.contains(d)) {
          ("","")
        } else {
          if (importTradesMap.get(d)) {
            ("Imported", "")
          } else {
            ("No Changes", "")
          }
        }
      }
      case None => ("","")
    }
  }

  def tradeIDFor(desk:Desk, text:String):TradeIDLabel = {
    val tradeText = text.trim.toUpperCase
    deskTradeSets(desk).flatMap( tradeSet => tradeSet.tradeIDFor(tradeText).toList) match {
      case Nil => {
        if (text.isEmpty) {
          throw new UnrecognisedTradeIDException("Please enter a trade id")
        } else {
          throw new UnrecognisedTradeIDException("No trade found for " + text)
        }
      }
      case id::Nil => TradeIDLabel(id.id, label(id.tradeSystem))
      case ids => throw new UnrecognisedTradeIDException("Ambigious trade id " + ids)
    }
  }
  def version = versionInfo

  def deskCloses = tradeStores.closedDesks.closedDesksByDay
  def latestTradeTimestamp(desk:Desk):TradeTimestamp = tradeStores.closedDesks.latestTradeTimestamp(desk)

  def intradayLatest = tradeStores.intradayTradeStore.intradayLatest
  def clearCache {reportService.clearCache()}

  def selectLiveAndErrorTrades(day: Day, timestamp: Timestamp, desk: Desk, tradePredicate: TradePredicate):List[Trade] = {
    deskTradeSets(Some(desk), tradePredicate).flatMap(_.selectLiveAndErrorTrades(day, timestamp))
  }

  def referenceDataTables() = referenceData.referenceDataTables()
  def referencePivot(table: ReferenceDataLabel, pivotFieldParams: PivotFieldParams) = referenceData.referencePivot(table, pivotFieldParams)
  def ukBusinessCalendar = ukHolidayCalendar
  def whoAmI = User.currentlyLoggedOn
  def allUserNames:List[String] = userSettingsDatabase.allUsersWithSettings
  def isStarlingDeveloper = {
    if (version.production) {
      User.currentlyLoggedOn.groups.contains(Groups.StarlingDevelopers)
    } else {
      val groups = User.currentlyLoggedOn.groups
      groups.contains(Groups.StarlingDevelopers) || groups.contains(Groups.StarlingTesters)
    }
  }

  def traders = allTraders.bookMap

  def orgPivot(pivotFieldParams:PivotFieldParams) = {
    PivotTableModel.createPivotData(new OrgPivotTableDataSource, pivotFieldParams)
  }

  def userStatsPivot(pivotFieldParams:PivotFieldParams):PivotData = {
    val table = "PageViewLog"
    val pageText = "Page Text"
    val name = "Name"
    val timestamp = "Timestamp"
    val time = "time"
    val dayName = "Day"
    val countName = "Count"
    val columns = {
      List(("Stat Fields", List(
        StringColumnDefinition("User Name", "loginName", table),
        StringColumnDefinition(name, "userName", table),
        StringColumnDefinition(pageText, "text", table),
        StringColumnDefinition("Short Page Text", "shortText", table),
        StringColumnDefinition("Page toString", "pageString", table),
        TimestampColumnDefinition(timestamp, time, table),
        new FieldBasedColumnDefinition(dayName, time, table) {
          def read(resultSet:ResultSetRow) = {
            Day.fromMillis(resultSet.getTimestamp(alias).instant)
          }
          override def filterClauses(values:Set[Any]) = {
            values.asInstanceOf[Set[Day]].map(d => ("YEAR(time)" eql d.year) and ("MONTH(time)" eql d.month) and ("DAY(time)" eql d.dayNumber)).toList
          }
        },
        new FieldBasedColumnDefinition("Month", time, table) {
          def read(resultSet:ResultSetRow) = {
            val day = Day.fromMillis(resultSet.getTimestamp(alias).instant)
            Month(day.year, day.month)
          }
          override def filterClauses(values:Set[Any]) = {
            values.asInstanceOf[Set[Month]].map(m => ("YEAR(time)" eql m.y) and ("MONTH(time)" eql m.m)).toList
          }
        },
        new ColumnDefinition(countName) {
          val alias = "t_count"
          def read(resultSet:ResultSetRow) = resultSet.getInt(alias)
          def filterClauses(values:Set[Any]) = List(FalseClause) //not supported
          override def fieldDetails = new SumIntFieldDetails(name)
          def selectFields = List("count(*) " + alias)
          def orderByFields = List()
          def groupByFields = List()
        }
        )))}

    val dayField = Field(dayName)
    val latestDays: Set[Any] = (Day.today - 2 upto Day.today).toList.toSet

    PivotTableModel.createPivotData(new OnTheFlySQLPivotTableDataSource(
      userSettingsDatabase.db,
      columns,
      From(RealTable(table), List()),
      List(),
      PivotFieldsState(rowFields = List(dayField, Field(name), Field(pageText)), dataFields = List(Field(countName)), filters = (dayField, SomeSelection(latestDays)) :: Nil ),
      List()
    ), pivotFieldParams)
  }

  def storeSystemInfo(info:OSInfo) {userSettingsDatabase.storeSystemInfo(User.currentlyLoggedOn, info)}

  def saveUserReport(reportName:String, data:UserReportData, showParameters:Boolean) {
    userSettingsDatabase.saveUserReport(User.currentlyLoggedOn, reportName, data, showParameters)
  }
  def deleteUserReport(reportName:String) {userSettingsDatabase.deleteUserReport(User.currentlyLoggedOn, reportName)}

}