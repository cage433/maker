package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}


/**
 * Small addition to the rabbit messages table to allow marking of events as processed and any recording of errors
 */
class Patch132_AddStatusAndErrorFieldsToRabbitMessages extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch132_AddStatusAndErrorFieldsToRabbitMessages.sql"))
  }
}
