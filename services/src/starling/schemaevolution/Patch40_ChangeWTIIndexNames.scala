package starling.schemaevolution

import starling.db.DBWriter
import system.Patch
import starling.richdb.RichDB
import starling.market.FuturesFrontPeriodIndex
import starling.services.StarlingInit

class Patch40_ChangeWTIIndexNames extends Patch {
//  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
//    val index1 = FuturesFrontPeriodIndex.WTI10
//    val index2 = FuturesFrontPeriodIndex.WTI20
//
//    for (table <- List("trinitytrade", "galenatrade", "instrument")) {
//      writer.update("update " + table + " set market = '" + index1.name.s + "' where market = 'WTI 1st Pos'")
//      writer.update("update " + table + " set market = '" + index2.name.s + "' where market = 'WTI 2nd Pos'")
//    }
//  }

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = null

  def patchDescription = "Change Trinity Index name of WTI indexes"
}