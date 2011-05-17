package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit


class Patch14_RemoveTradesFromWrongTradeTables extends Patch {
  def patchDescription = "Removing Galena trades from the Trinity table and vice-versa"

  val metalsPrefixes = List("AGM", "BVM", "DLM", "DLU", "HMC", "TTS", "XCO")
  val nonMetalsPrefixes = List("AZR", "GAL", "GEF", "LOF")

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("delete from GalenaTrade where substring(portfolio, 1, 3) in (:portfolios)", Map("portfolios" -> metalsPrefixes))
    writer.update("delete from TrinityTrade where substring(portfolio, 1, 3) in (:portfolios)", Map("portfolios" -> nonMetalsPrefixes))
  }
}