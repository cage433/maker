package starling.gui.api

import collection.immutable.SortedSet
import starling.daterange.TimeOfDay

case class UserReport(reportName:String, data:UserReportData, showParameters:Boolean)
case class UserReportData(
        baseDayTimeOfDay:TimeOfDay,
        tradeSelection:TradeSelection,
        marketDataSelection:MarketDataSelection,
        environmentModifiers:SortedSet[EnvironmentModifierLabel],
        reportOptions:ReportOptions,
        environmentRule:EnvironmentRuleLabel,
        valuationDayOffset:Int,
        valuationDayTimeOfDay:TimeOfDay,
        thetaDayOffset:Int,
        thetaDayTimeOfDay:TimeOfDay,
        tradeVersionOffSetOrLive:Either[Int,Boolean],
        liveOnOffSet:Either[Int,Boolean],
        pnl:Option[(Int,Either[Int,Boolean],Boolean,TimeOfDay)]
    )