package starling.gui

import api._
import starling.pivot.{ExtraFormatInfo, OtherLayoutInfo, PivotFieldsState}
import starling.browser.internal.Key


object StandardUserSettingKeys {
  val bundle = "StarlingServer"
  val InitialTradeSelection = new Key[(Option[Desk], Option[IntradayGroups])]("The initial selection for the trade selection page", bundle)
  val IntradayGroupsDefault = new Key[List[String]]("IntradayGroupsDefault", bundle) //Used when Excel is not checked
  val DeskDefault = new Key[Desk]("DeskDefault", bundle) //Used when desk is not checked
  val InitialMarketDataSelection = new Key[MarketDataSelection]("The initial market data selection", bundle)
  val PricingGroupDefault = new Key[PricingGroup]("PricingGroupDefault", bundle) //Used when pricing group is not checked
  val ExcelMarketDataDefault = new Key[String]("ExcelMarketDataDefault", bundle) //Used when excel market data is not checked
  val DefaultReportFields = new Key[(PivotFieldsState,OtherLayoutInfo)]("DefaultReportFields", bundle)
  val ExtraFormattingInfo = new Key[ExtraFormatInfo]("ExtraFormattingInfo", bundle)
  val UserMarketDataTypeLayout = new Key[Map[MarketDataTypeLabel,PivotFieldsState]]("UserMarketDataTypeLayout", bundle)
}