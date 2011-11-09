package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}
import xml.transform.RewriteRule
import xml.{Node, Text, Elem}

class Patch139_RenameSFStoSHFE extends Patch {
  protected def runPatch(init: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("update Markets set exchange = 'SHFE' where exchange = 'SFS'")
    ChangeMarketDataValueKeys.fixMarketDataExtendedKey(starling, writer, Fix)
  }
  object Fix extends RewriteRule {
    override def transform(n: Node) = n match {
      case Text("SFS") => Text("SHFE")
      case o => o
    }
  }
}