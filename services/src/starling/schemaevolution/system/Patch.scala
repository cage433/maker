package starling.schemaevolution.system

import java.sql.Connection
import java.util.regex.Pattern
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.services.StarlingInit
import starling.props.Props
import scalaz.Scalaz._


object Patch {
  def patchProps(patchClass: Class[_]): (Int, String) = {
    val fullClassName = patchClass.getName
    val patchClassName = fullClassName.substring(fullClassName.lastIndexOf(".") + 1)

    //Create a regular expression to get the patch number and the description string
    val pattern = Pattern.compile("""^Patch(\d+)_?(.*)$""")
    val matcher = pattern.matcher(patchClassName)

    //Extract the patch number and the description if they exist
    matcher.find()
    val patchNumber = matcher.group(1)
    val patchName = matcher.group(2)

    //Ensure patch has a name and a number
    if(patchNumber == "" || patchName == "") {
      throw new RuntimeException("Patch [" + fullClassName + "] does have a valid name of Patch[PatchNumber(Int)]_[PatchName(String)] eg Patch1_CreateTables")
    }

    //Return the tuple of patch number and description
    (patchNumber.toInt, patchName)
  }
}

trait Patch {

  override def toString = patchNumber+":"+patchName

  val (patchNumber, patchName) = Patch.patchProps(getClass)

  def deferredReason(context: PatchContext): Exception = throw new Exception("Removed")

  final def applyPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    //Run the actual SQL script to update the schema and check completed successfully
    runPatch(starlingInit, starling, writer)

    //Check the patch has run successfully
    checkRunPatchCompletedSuccessfully(starling)
  }


  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter)

  //Override this method if you want to double check that a patch has been applied successfully throw a RuntimeException if it doesn't
  protected def checkRunPatchCompletedSuccessfully(starling: RichDB) = {
  }

  def requiresRestart = false
}

class PatchContext(val props: Props, appliedPatches: Set[String]) {
  def dependsOn[T <: Patch](implicit m: Manifest[T]): Option[String] = dependsOn(Patch.patchProps(m.erasure)._2)

  private def dependsOn(patchName: String): Option[String] =
    appliedPatches.contains(patchName) ? none[String] | some("Depends on " + patchName)
}