package starling.services.trade.instrumentreaders

import starling.models._
import starling.instrument._
import starling.daterange._
import starling.quantity.Quantity
import starling.services.trade.ExcelRow
import starling.market._
import starling.utils.ImplicitConversions._
import formula.InvalidFormulaIndexException


trait ExcelInstrumentReader {
  /**
   * Override this method and return true if the reader can handle this row
   */
  def canRead(row: ExcelRow): Boolean

  /**
   * Given the row create the instrument. This is only called if canHandle returned true.
   */
  def create(row: ExcelRow): Tradeable
}

case class ExcelInstrumentReaderException(s: String) extends Exception(s)

object ExcelInstrumentReader {

  import starling.market.Market._

  /**
   * aliases between what the traders put in the blotter and what we call the instruments
   */
  val instrumentAliases = Map("swap" -> CommoditySwap, "cso" -> CalendarSpreadOption, "option" -> FuturesOption,
    "asian" -> AsianOption, "apo" -> AsianOption)

  /**
   * aliases between what the traders put in the blotter and what we call the markets
   */
  val marketAliases: Map[String, Market] = {
    import Market._
    import FuturesSpreadMarket._

    def alias(aliases: (Market, String)*): Map[String, Market] = {
      aliases.toList.filter {
        case (market, alias) => market.name == alias
      }.map(_._2).update {
        redundantAliases => require(redundantAliases.isEmpty, "These market aliases are redundant: " + redundantAliases)
      }

      def conflicts(market: Market, alias: String) = {
        Market.all.find(_.name == alias).map(f => f != market).getOrElse(false)
      }

      aliases.toList.filter {
        case (market, alias) => conflicts(market, alias)
      }.map(_._2).update {
        conflictingAliases => require(conflictingAliases.isEmpty, "These market aliases conflict: " + conflictingAliases)
      }

      val combined = aliases.map {
        case (market, alias) => (alias.toLowerCase, market)
      }.toMap ++ Market.all.toMapWithKeys(_.name.toLowerCase)

      val renamedIPE = combined.filter(_._1.contains("ipe")).map{
        case (name, market) => (name.replaceAll("ipe", "ice") -> market)
      }

      combined ++ renamedIPE
    }

    alias(
      ICE_BRENT → "brent",
      NYMEX_WTI → "wti",
      RB_CRACKS -> "rb cracks",
      RB_CRACKS -> "nymex rbob vs ipe brent",
      RB_BRENT_CRACKS -> "rb brent cracks",
      RBHO -> "rbho",
      GO_CRACKS -> "go cracks",
      GO_CRACKS -> "ipe gas oil vs ipe brent",
      ICE_WTI_BRENT -> "wti brent",
      ICE_WTI_BRENT -> "ice wti vs ice brent",
      NYMEX_WTI_BRENT -> "ny wti brent"
    )
  }

  def marketOption(marketName: String): Option[Market] = marketAliases.get(marketName.toLowerCase)
  def commodityMarketOption(marketName: String): Option[CommodityMarket] = marketAliases.get(marketName.toLowerCase).asInstanceOf[Option[CommodityMarket]]

  /**
   * aliases between what the traders put in the blotter and what we call the indexes
   */
  val indexAliases: Map[String, Index] = Map("gas oil crack" -> Index.IPE_GAS_OIL_VS_IPE_BRENT) ++ Index.all.map(i => i.name.toLowerCase -> i).toMap

  def indexOption(indexName: String) = indexAliases.get(indexName.toLowerCase)
}