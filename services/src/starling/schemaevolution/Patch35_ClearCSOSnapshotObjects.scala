package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.dbx.QueryBuilder
import starling.curves.SpreadStdDevSurfaceDataType
import starling.services.StarlingInit

/**
 * Class definition of SpreadStdDevSurfaceData has changed. But since it's a very new class
 * and hasn't been used in production yet, seems easier to just wipe out any historical
 * instances.
 */
class Patch35_ClearCSOSnapshotObjects extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    import QueryBuilder._
    writer.delete("SnapshotData", "dataTypeKey" eql SpreadStdDevSurfaceDataType.name)
  }

  def patchDescription = "Clear all XStream CSO spread objects from DB - class definition has changed."
}