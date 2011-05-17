package starling.schemaevolution

import starling.richdb.RichDB
import system.{PatchUtils, Patch}
import starling.db.DBWriter
import starling.services.StarlingInit

/**
 * Creates a database for storing the uncommitted data that is shared between users of the new
 * forward curve app. When committed, the data is pushed to the FC database, and isn't stored
 * here any more.
 *
 * Schema: Each user has their own "space" which holds their "unsaved" changes. When they "save"
 * this is pushed into the staging space for their pricing group, making it visible to everyone
 * else in that staging pricing group.
 */
class Patch18_CreateForwardCurveStagingDatabase extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    writer.update(PatchUtils.getFileFromClasspath("/schemaevolution/Patch18_CreateForwardCurveStagingDatabase.sql"))
  }

  def patchDescription = "Create staging database for collaborating on forward curves."
}