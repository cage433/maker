package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit

class Patch24_RenameTrinityRevalGroups extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val renames = Map(
      "Oil" -> "Galena/Live",
      "Metals" -> "Trinity/Live",
      "GMA" -> "Galena/Full Curve",
      "Freight" -> "Trinity/Live" 
     )

    renames.foreach{
      case (old, newName) =>
        writer.update(
          """
            update Snapshot set revalGroup = '%s'
            where
            revalGroup = '%s'
          """ % (newName, old))
    }

  }

  def patchDescription = "Rename trinity reval groups"
}