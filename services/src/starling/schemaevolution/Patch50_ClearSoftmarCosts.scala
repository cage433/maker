package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch50_ClearSoftmarCosts extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update("""update SoftMarTrade set costs = null""")
  }

  def patchDescription = "Clear Softmar costs, they are incorrect and will not be fixed any time soon"
}