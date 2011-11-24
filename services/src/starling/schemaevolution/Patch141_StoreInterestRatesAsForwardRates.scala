package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.marketdata.ForwardRateDataType


class Patch141_StoreInterestRatesAsForwardRates extends Patch {
  protected def runPatch(init: StarlingInit, db: RichDB, writer: DBWriter) = {
    // The existing forward rates can be deleted
    val patchUtil = MarketDataPatchUtil(db, writer)
    patchUtil.deleteMarketData(removeOrphaned = false, types = ForwardRateDataType.name)

    writer.update("DELETE FROM MarketDataValue WHERE extendedKey IN (" +
      "SELECT DISTINCT id FROM MarketDataExtendedKey WHERE observationTime = 'Libor Close')")

    patchUtil.removeOrphanedMarketDataValueKeys
  }
}