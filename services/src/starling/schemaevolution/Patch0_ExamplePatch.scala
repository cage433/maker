package starling.schemaevolution


import starling.schemaevolution.system.Patch
import java.sql.Connection
import starling.db.DBWriter
import starling.richdb.{RichDB}
import starling.services.StarlingInit

//Patch names must follow the format Patch[PatchNumber(int)]_[PatchDescription(string)] for example Patch1_CreateDatabase
//Patches must also reside in the starling.schemaevolution package
class Patch0_ExamplePatch extends Patch {

  /////////
  //REQURED
  /////////

  //The following method must be overriden to provide a description of the patch which is stored in the database in the database

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = null
}