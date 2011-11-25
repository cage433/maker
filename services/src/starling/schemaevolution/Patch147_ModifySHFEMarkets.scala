package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter


class Patch147_ModifySHFEMarkets extends Patch {
  override def requiresRestart = true // because this patch modifies markets which may have been read already.

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.insert("Markets", Map(
      "Name" → "Shanghai Lead", "type" → "FuturesMarket", "uom" → "MT", "ccy" → "CNY", "businessCalendar" → "SFS",
      "lotSize" → "Some(5.0)", "tenor" → "Month", "commodity" → "Lead", "limSymbol" → "Some(PB)", "limMultiplier" → "None",
      "defaultPrecision" → "Some(4)", "clearPortPrecision" → "Some(2)", "expiryRule" → "SFS", "exchange" → "SHFE"
    ))

    writer.update("update Markets set limSymbol = 'Some(RB)' where eaiQuoteID = 1441")
  }
}