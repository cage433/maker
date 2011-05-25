package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.services.StarlingInit

/**
 * We have deleted instruments in the middle of many trades' version history. This is almost certainly
 * due to errors on our part - e.g. importing from the wrong trade system 
 */
class Patch39_RemoveIncorrectTradeDeletions extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    List("EAITrade", "GalenaTrade", "RefinedAssignment", "RefinedFixation", "SoftmarTrade", "TrinityTrade").foreach {
      table =>
        starling.query("select count(*) as cnt from %s where instrument = 'DeletedInstrument' and timestampTo_Cache is not null" % table){
          rs =>
            println("About to delete " + rs.getInt("cnt") + " rows from " + table)
        }
        starling.inTransaction {
          newWriter => {
            writer.update("delete from %s where instrument = 'DeletedInstrument' and timestampTo_Cache is not null" % table)
          }
        }
    }
  }

  def patchDescription = "Remove DeletedInstruments that aren't the last version of a trade"
}