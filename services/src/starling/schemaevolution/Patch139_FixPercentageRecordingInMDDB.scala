package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
class Patch139_FixPercentageRecordingInMDDB extends Patch {

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    // percentages are now recorded as (100, %) for 100% instead of (1.0, %) for 100%
    val scale = """
    update mdv
    set value = value * 100.0
    from MarketDatavalue mdv
    inner join MarketDataExtendedKey mdek on mdek.id = mdv.extendedKey
    where uom = '%'
    and marketDataType != 'Price'
    """
    writer.update(scale)
  }
}
