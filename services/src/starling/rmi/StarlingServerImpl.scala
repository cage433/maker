package starling.rmi

import collection.immutable.{Set, List}

import starling.calendar.BusinessCalendarSet
import starling.daterange._
import starling.db._
import starling.gui.api._
import starling.pivot._
import starling.pivot.model._
import starling.services._
import starling.utils._
import starling.dbx.{FalseClause, From, RealTable}

import starling.dbx.QueryBuilder._
import starling.browser.service.Version
import starling.auth.{Groups, LdapUserLookup, User}

class StarlingServerImpl(
        val name:String,
        userSettingsDatabase:UserSettingsDatabase,
        versionInfo:Version,
        referenceData:ReferenceDataService,
        ukHolidayCalendar: BusinessCalendarSet
      ) extends StarlingServer with Log {

  def init() = StarlingServerInitialData(
    name,
    versionInfo,
    ukHolidayCalendar,
    User.currentlyLoggedOn,
    isStarlingDeveloper,
    userSettingsDatabase.allUsersWithSettings
  )

  private def isStarlingDeveloper = {
    if (versionInfo.production) {
      User.currentlyLoggedOn.groups.contains(Groups.StarlingProductionAdmin)
    } else {
      val groups = User.currentlyLoggedOn.groups
      import Groups._
      List(StarlingDevelopers, StarlingTesters, StarlingProductionAdmin, StarlingBusinessUsers).exists(groups.contains(_))
    }
  }

  def referenceDataTables() = referenceData.referenceDataTables()
  def referencePivot(table: ReferenceDataLabel, pivotFieldParams: PivotFieldParams) = referenceData.referencePivot(table, pivotFieldParams)

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

  def gitLog(pivotFieldParams:PivotFieldParams, numCommits:Int) = {
    val gitPivotSource = new GitPivotDataSource(numCommits)
    PivotTableModel.createPivotData(gitPivotSource, pivotFieldParams)
  }
}
