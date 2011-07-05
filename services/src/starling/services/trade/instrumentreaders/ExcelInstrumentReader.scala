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
  val marketAliases: Map[String, CommodityMarket] = {
    import Market._

    def alias(aliases: (CommodityMarket, String)*): Map[String, CommodityMarket] = {
      aliases.toList.filter { case (market, alias) => market.name == alias }.map(_._2).update {
        redundantAliases => require(redundantAliases.isEmpty, "These market aliases are redundant: " + redundantAliases)
      }

      def conflicts(market: CommodityMarket, alias: String) = {
        Market.all.find(_.name == alias).map(f => f != market).getOrElse(false)
      }

      aliases.toList.filter { case (market, alias) => conflicts(market, alias) }.map(_._2).update {
        conflictingAliases => require(conflictingAliases.isEmpty, "These market aliases conflict: " + conflictingAliases)
      }

      aliases.map { case (market, alias) => (alias.toLowerCase, market) }.toMap ++ Market.all.toMapWithKeys(_.name.toLowerCase)
    }

    alias(
      ICE_BRENT                               → "brent",
      NYMEX_WTI                               → "wti"
    )
  }

  def marketOption(marketName: String) = marketAliases.get(marketName.toLowerCase)

  /**
   * aliases between what the traders put in the blotter and what we call the indexes
   */
  val indexAliases: Map[String, Index] = Map("gas oil crack" -> Index.IPE_GAS_OIL_VS_IPE_BRENT) ++ Index.all.map(i => i.name.toLowerCase -> i).toMap

  def indexOption(indexName: String) = indexAliases.get(indexName.toLowerCase)
}