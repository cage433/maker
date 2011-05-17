package starling.gui

import api._
import starling.pivot.PivotLayout
import starling.daterange.{Day, Timestamp}
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.eai.Book
import starling.utils.CaseInsensitive

object LocalCacheKeys {
  val UserPivotLayouts                    = new Key[List[PivotLayout]]("The pivot layouts saved by the user")
  val UserReports                         = new Key[List[UserReport]]("The reports saved by the user")
  val PricingGroups                       = new Key[List[PricingGroup]]("PricingGroups")
  val ExcelDataSets                       = new Key[List[String]]("ExcelDataSets")
  val Snapshots                           = new Key[Map[MarketDataSelection,List[SnapshotIDLabel]]]("Snapshots")
  val ObservationDaysForPricingGroup      = new Key[Map[PricingGroup,Set[Day]]]("ObservationDaysForPricingGroup")
  val ObservationDaysForExcel             = new Key[Map[String,Set[Day]]]("ObservationDaysForExcel")
  val PricingGroupLatestMarketDataVersion = new Key[Map[PricingGroup,Int]]("PricingGroupLatestMarketDataVersion")
  val ExcelLatestMarketDataVersion        = new Key[Map[String,Int]]("ExcelLatestMarketDataVersion")
  val ReportOptionsAvailable              = new Key[ReportOptionsAvailable]("ReportOptionsAvailable")
  val AllNotifications                    = new Key[List[Notification]]("AllNotifications")
  val UserNotifications                   = new Key[List[Notification]]("UserNotifications")
  val Version                             = new Key[Version]("Version")
  val TradersBookLookup                   = new Key[Map[User,(Book,Desk)]]("TradersBookLookup")
  val DeskCloses                          = new Key[Map[Desk, Map[Day,List[TradeTimestamp]]]]("DeskCloses")
  val IntradayLatest                      = new Key[Map[String, (User, Timestamp)]]("IntradayLatest")
  val CurrentUser                         = new Key[User]("CurrentUser")
  val UKBusinessCalendar                  = new Key[BusinessCalendar]("UKHolidays")
  val AllUserNames                        = new Key[List[String]]("AllUserNames")
  val Desks                               = new Key[List[Desk]]("Desks")
  val GroupToDesksMap                     = new Key[Map[CaseInsensitive, Set[Desk]]]("GroupToDesksMap")
  val IsStarlingDeveloper                 = new Key[Boolean]("IsStarlingDeveloper")
  val EnvironmentRules                    = new Key[Map[PricingGroup,List[EnvironmentRuleLabel]]]("EnvironmentRules")
  val CurveTypes                          = new Key[List[CurveTypeLabel]]("CurveTypes")
}

