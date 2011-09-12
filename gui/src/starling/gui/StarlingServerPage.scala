package starling.gui

import api._
import pages.{TimestampChooser}
import starling.rmi.StarlingServer
import starling.daterange.{Day, Timestamp}
import collection.immutable.TreeSet
import collection.SortedSet
import starling.auth.User
import starling.browser._
import service.internal.HeterogeneousMap

trait StarlingServerPage extends Page {
  def bundle = "StarlingServer"
  type SC = StarlingServerContext
  def createServerContext(sc:ServerContext) = new StarlingServerContext(sc.lookup(classOf[StarlingServer]))
}
trait StarlingBookmark extends Bookmark {
  def createStarlingPage(day:Option[Day], serverContext:StarlingServerContext, context:PageContext):Page
  def createPage(day:Option[BrowserDay], serverContext:ServerContext, context:PageContext):Page = {
    val realDay = day.map( d => Day(d.year, d.month, d.dayOfMonth))
    createStarlingPage(realDay, new StarlingServerContext(serverContext.lookup(classOf[StarlingServer])), context)
  }
}

object StarlingLocalCache {
  implicit def localCache2StarlingLocalCache(cache:LocalCache) = StarlingLocalCache(cache.localCache)
}
case class StarlingLocalCache(localCache:HeterogeneousMap[LocalCacheKey]) {
  import starling.gui.LocalCacheKeys._
  def pricingGroups(maybeDesk:Option[Desk]):List[PricingGroup] = localCache(PricingGroups).intersect(validGroups(maybeDesk))
  def excelDataSets = localCache(ExcelDataSets)
  def currentUser = localCache(CurrentUser)

  def latestTimestamp(desk: Desk): Option[TradeTimestamp] = deskCloses(desk).sortWith(_.timestamp > _.timestamp).headOption
  def latestTimestamp(groups: IntradayGroups): Timestamp = {
    val dTS = TimestampChooser.defaultUnitialisedValue.closeDay.toTimestamp
    // A path can be passed in so in that case get the max value of all it's children.
    val map = localCache(IntradayLatest)
    val availableGroups = map.keySet
    val groupsToUse = groups.subgroups.flatMap(g => {
      if (availableGroups.contains(g)) {
        List(g)
      } else {
        availableGroups.filter(_.startsWith(g + "/")).toList
      }
    })
    groupsToUse.map(n => localCache(IntradayLatest).getOrElse(n, (User.Dev, dTS))._2) match {
      case Nil => dTS
      case l => l.max
    }
  }

  def deskCloses(desk: Option[Desk]): List[TradeTimestamp] = desk.map(deskCloses).getOrElse(Nil)
  def deskCloses(desk: Desk): List[TradeTimestamp] = {
    localCache(DeskCloses).get(desk).map(closes => closes.values.flatten.toList.sortWith(_.timestamp > _.timestamp)).getOrElse(Nil)
  }

  def traderBookLookup: Map[User, List[Desk]] = localCache(TradersBookLookup)

  def curveTypes = localCache(CurveTypes)

  def intradaySubgroups = localCache(IntradayLatest)


  def snapshots(maybeDesk:Option[Desk]) = {
    val vg = validGroups(maybeDesk)
    localCache(Snapshots).filter{ case(MarketDataSelection(pg, excel), snapshots) => !pg.isDefined || vg.contains(pg.get) }
  }
  def environmentRulesForPricingGroup(pricingGroup:Option[PricingGroup]) = {
    pricingGroup match {
      case Some(pg) => localCache(EnvironmentRules)(pg)
      case None => localCache(EnvironmentRules).values.flatten.toSet.toList
    }
  }

  def populatedDays(selection:MarketDataSelection):SortedSet[Day] = {
    val pricingGroupDays = selection.pricingGroup match {
      case Some(pg) => populatedObservationDaysForPricingGroup.getOrElse(pg, Set())
      case None => Set()
    }
    val excelDays = selection.excel match {
      case Some(name) => populatedObservationDaysForExcel.getOrElse(name, Set())
      case None => Set()
    }
    TreeSet[Day]() ++ pricingGroupDays ++ excelDays
  }
  def populatedObservationDaysForPricingGroup = localCache(ObservationDaysForPricingGroup)
  def populatedObservationDaysForExcel = localCache(ObservationDaysForExcel)
  def latestMarketDataVersion(selection:MarketDataSelection) = latestMarketDataVersionIfValid(selection).get
  def latestMarketDataVersionIfValid(selection:MarketDataSelection) = {
    if (selection.excel.isDefined && !localCache(ExcelLatestMarketDataVersion).contains(selection.excel.get)) {
      None
    } else {
      val versions = selection.pricingGroup.toList.map { pg =>
        localCache(PricingGroupLatestMarketDataVersion)(pg)
      } ::: selection.excel.toList.map { excel =>
        localCache(ExcelLatestMarketDataVersion)(excel)
      }
      Some(if (versions.isEmpty) 0 else versions.max)
    }
  }
  def reportOptionsAvailable = localCache(ReportOptionsAvailable)
  def ukBusinessCalendar = localCache(UKBusinessCalendar)
  def desks = localCache(Desks)
  def groupToDesksMap = localCache(GroupToDesksMap)
  def isStarlingDeveloper = localCache(IsStarlingDeveloper)

  private def validGroups(maybeDesk:Option[Desk]) = maybeDesk.map(_.pricingGroups).getOrElse(PricingGroup.values)
}