package starling.gui.api

import collection.immutable.SortedSet
import starling.daterange.TimeOfDay

case class UserReport(reportName:String, data:UserReportData, showParameters:Boolean)
case class UserReportData(
        tradeSelection:TradeSelection,
        marketDataSelection:MarketDataSelection,
        environmentModifiers:SortedSet[EnvironmentModifierLabel],
        reportOptions:ReportOptions,
        environmentRule:EnvironmentRuleLabel,
        valuationDayOffset:Int,
        valuationDayTimeOfDay:TimeOfDay,
        thetaDayOffset:Int,
        thetaDayTimeOfDay:TimeOfDay,
        tradeVersionOffSetOrLive:Int,
        liveOnOffSet:Int,
        pnl:Option[(Int,Int,Boolean,TimeOfDay)]
    )
