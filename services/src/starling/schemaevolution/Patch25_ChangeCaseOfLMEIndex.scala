package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit

class Patch25_ChangeCaseOfLMEIndex extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(
      """
        update trinityTrade set FixingIndex = 'Lead low 4 LME'
        where FixingIndex = 'Lead Low 4 LME'
      """)

  }

  def patchDescription = "Change case of Lead Low 4 LME to match Trinity "
}