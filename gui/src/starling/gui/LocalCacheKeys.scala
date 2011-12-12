package starling.gui

import api._
import starling.pivot.PivotLayout
import starling.daterange.{Day, Timestamp}
import starling.auth.User
import starling.calendar.BusinessCalendar
import starling.utils.CaseInsensitive
import starling.browser.LocalCacheKey
import org.joda.time.DateTime

object LocalCacheKeys {
  val PricingGroups                       = new LocalCacheKey[List[PricingGroupDefinition]]("PricingGroups")
  val ExcelDataSets                       = new LocalCacheKey[List[String]]("ExcelDataSets")
  val Snapshots                           = new LocalCacheKey[Map[MarketDataSelection,List[SnapshotIDLabel]]]("Snapshots")
  val ObservationDaysForPricingGroup      = new LocalCacheKey[Map[PricingGroup,Set[Day]]]("ObservationDaysForPricingGroup")
  val ObservationDaysForExcel             = new LocalCacheKey[Map[String,Set[Day]]]("ObservationDaysForExcel")
  val PricingGroupLatestMarketDataVersion = new LocalCacheKey[Map[PricingGroup,Int]]("PricingGroupLatestMarketDataVersion")
  val ExcelLatestMarketDataVersion        = new LocalCacheKey[Map[String,Int]]("ExcelLatestMarketDataVersion")
  val ReportOptionsAvailable              = new LocalCacheKey[ReportOptionsAvailable]("ReportOptionsAvailable")
  val DeskCloses                          = new LocalCacheKey[Map[Desk, Map[Day,List[TradeTimestamp]]]]("DeskCloses")
  val IntradayLatest                      = new LocalCacheKey[Map[String, (User, Timestamp)]]("IntradayLatest")
  val UKBusinessCalendar                  = new LocalCacheKey[BusinessCalendar]("UKHolidays")
  val Desks                               = new LocalCacheKey[List[Desk]]("Desks")
  val GroupToDesksMap                     = new LocalCacheKey[Map[CaseInsensitive, Set[Desk]]]("GroupToDesksMap")
  val IsStarlingDeveloper                 = new LocalCacheKey[Boolean]("IsStarlingDeveloper")
  val EnvironmentRules                    = new LocalCacheKey[Map[PricingGroup,List[EnvironmentRuleLabel]]]("EnvironmentRules")
  val CurveTypes                          = new LocalCacheKey[List[CurveTypeLabel]]("CurveTypes")
  val CurrentUser                         = new LocalCacheKey[User]("currentUser")
  val LatestRabbitEvent                   = new LocalCacheKey[Long]("LatestRabbitEvent")
  val LatestEmailEvent                    = new LocalCacheKey[Timestamp]("LatestEmailEvent")
  val MethodLogIndex                      = new LocalCacheKey[Int]("MethodLogIndex")
}

