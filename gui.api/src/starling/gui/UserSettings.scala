package starling.gui

import api._
import starling.utils.HeterogeneousMap
import java.awt.Rectangle
import reflect.Manifest
import starling.pivot.{ExtraFormatInfo, OtherLayoutInfo, PivotFieldsState}

case class ManifestedKey[K](manifest:String, key:Key[K])
object ManifestedKey {
  def apply[K](manifest:Manifest[K], key:Key[K]):ManifestedKey[K] = ManifestedKey(manifest.toString, key)
}

@serializable
class UserSettings {
  val map = new HeterogeneousMap[ManifestedKey]

  def settingExists[T](key: Key[T])(implicit m: Manifest[T]):Boolean = {
    map.contains(ManifestedKey(m,key))
  }

  def getSetting[T](key: Key[T], default: => T)(implicit m: Manifest[T]): T = {
    map.getOrElse(ManifestedKey(m,key), default)
  }

  def getSetting[T](key: Key[T])(implicit m: Manifest[T]): T = {
    map(ManifestedKey(m,key))
  }

  def getSettingOption[T](key: Key[T])(implicit m: Manifest[T]): Option[T] = {
    map.get(ManifestedKey(m,key))
  }

  def putSetting[T](key: Key[T], value: T)(implicit m: Manifest[T]) {
    map(ManifestedKey(m,key)) = value
  }

  override def toString = map.toString()
}

/**
 * Key for user settings.
 * <P>
 * Because of a bug with the current version of scala, the description is effectively used as the hash map key so please
 * ensure they are unique.
 */
case class Key[T](description: String) {
  override def hashCode = description.hashCode

  override def equals(obj: Any) = obj match {
    case Key(desc) => desc == description
    case _ => false
  }
}

object StandardUserSettingKeys {
  val MainWindowBounds = new Key[Rectangle]("The location and size of the main window")
  val InitialTradeSelection = new Key[(Option[Desk], Option[IntradayGroups])]("The initial selection for the trade selection page")
  val IntradayGroupsDefault = new Key[List[String]]("IntradayGroupsDefault") //Used when Excel is not checked
  val DeskDefault = new Key[Desk]("DeskDefault") //Used when desk is not checked
  val InitialMarketDataSelection = new Key[MarketDataSelection]("The initial market data selection")
  val PricingGroupDefault = new Key[PricingGroup]("PricingGroupDefault") //Used when pricing group is not checked
  val ExcelMarketDataDefault = new Key[String]("ExcelMarketDataDefault") //Used when excel market data is not checked
  val DefaultReportFields = new Key[(PivotFieldsState,OtherLayoutInfo)]("DefaultReportFields")
  val ExtraFormattingInfo = new Key[ExtraFormatInfo]("ExtraFormattingInfo")
  val UserMarketDataTypeLayout = new Key[Map[MarketDataTypeLabel,PivotFieldsState]]("UserMarketDataTypeLayout")
  val LiveDefault = new Key[Boolean]("LiveDefault")

}