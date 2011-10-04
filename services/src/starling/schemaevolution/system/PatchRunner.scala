package starling.schemaevolution.system


import java.sql.{ResultSet, Connection}
import java.io.File
import starling.utils.Log
import starling.daterange.Timestamp
import starling.richdb.{RichDB}
import starling.db.DBWriter
import starling.dbx.QueryBuilder._
import starling.services.StarlingInit
import org.apache.commons.io.IOUtils
import starling.utils.ImplicitConversions._
import collection.immutable.Map
import collection.mutable.{Set => MSet}

//Note although there is a lot of error checking in this class one thing it does not check for is the possibility that
//there are more patches applied then actually exist in the patch package.  This is on purpose just in case we want to
//start clearing out old patches if the numbers start getting high.
class PatchRunner(starling: RichDB, readOnlyMode: Boolean, starlingInit: StarlingInit) {

  def main(args:Array[String]) {
    updateSchemaIfRequired
  }

  def updateSchemaIfRequired(): Boolean = {

    Log.info("SCHEMAEVOLUTION: Checking patch level and applying required patches if necessary")

    val alreadyAppliedPatches: MSet[String] = MSet() ++ getListOfAlreadyAppliedPatches(starling)

    val patchesToApply = getListOfAvailablePatchesFromClasspath.filter(
      patch => !alreadyAppliedPatches.contains(patch.patchName)
    )

   //If there are no patches to apply say so and return coz there is nothing to do
    if(patchesToApply.size == 0) {
      Log.info("SCHEMAEVOLUTION: No Patches to apply")
      return false
    }

    if (readOnlyMode) {
      Log.info("Should apply patches " + patchesToApply + " but in readonly mode")
      return false
    }

    //Sort the patches to apply
    val sortedPatchesToApply = patchesToApply.sortWith(_.patchNumber < _.patchNumber)

    //Apply the patches
    val allPatches = Log.infoWithTime("Applying any patches (" + sortedPatchesToApply.size + ")") {
      sortedPatchesToApply.toMapWithValues(p => {
        applyPatch(starlingInit, starling, p, new PatchContext(starlingInit.props, alreadyAppliedPatches.toSet))
          .update(identity, _ => alreadyAppliedPatches.add(p.patchName))
      })
    }

    val deferredPatches: Map[Patch, String] = allPatches.collectValues { case Left(Some(deferredReason)) => deferredReason }
    val requiresRestart = allPatches.exists { case (_, Right(true)) => true; case _ => false }

    deferredPatches.foreach { case (patch, deferredReason) =>
      Log.warn("====== PATCH DEFERRED: %s BECAUSE: %s ======" % (patch, deferredReason))
    }

    //Check that all the patches are applied now
    val currentlyAppliedPatches = Set() ++ getListOfAlreadyAppliedPatches(starling)
    val unAppliedPatches = patchesToApply.filter(patch => !currentlyAppliedPatches.contains(patch.patchName)).sortWith(_.patchNumber > _.patchNumber)
      .filterNot(deferredPatches.contains(_))

    if(unAppliedPatches.size != 0) {
      throw new RuntimeException("SCHEMAEVOLUTION: The following patches have not been applied correctly, cannot continue [" + unAppliedPatches + "]")
    }

    requiresRestart
  }

  private def getListOfAlreadyAppliedPatches(starlingDB:RichDB) = {
    starlingDB.queryWithResult( (select("patchName, patchNumber") from "SchemaEvolutionPatch")) { rs=>rs.getString("patchName")}
  }

  private def getListOfAvailablePatchesFromClasspath(): List[Patch] = {

    val packageDirectoryToSearch = "/starling/schemaevolution/"

    //Get a url to the package which should contain the patches
    val packageDirectoryURL = getClass.getResource(packageDirectoryToSearch)

    //Get a list of the contents
    val packageDirectoryContents = {
      IOUtils.readLines(packageDirectoryURL.openConnection().getInputStream).toArray(new Array[String](0)).toList
      //new File(packageDirectoryURL.toURI()).list().toList
    }

    //Remove anything which is not a class
    val packageDirectoryClasses = packageDirectoryContents.filter(className => className.endsWith(".class"))

    //Strip out any of the directory contents which have $'s in the name, we are not interested in these
    val packageDirectoryClassesWithNoInnerClasses = packageDirectoryClasses.filter(className => !className.contains("$"))

    //Remove the .class part of the file name
    val classes = packageDirectoryClassesWithNoInnerClasses.map(_.stripSuffix(".class"))

    //Remove any classes we are explicitly not interested in
    val explicitlyRemoveClasses = List("Patch0_ExamplePatch")
    val patchClasses = classes.filterNot(explicitlyRemoveClasses.contains(_)).filter(_.startsWith("Patch")) 
    //Function to instantiate all the classes
    def createListOfPatchClasses(patchClassesSoFar: List[Patch], remainingPatchClassNames: List[String]): List[Patch] = {
      remainingPatchClassNames match {
        case head::tail => {
          createListOfPatchClasses(Class.forName("starling.schemaevolution." + head).newInstance.asInstanceOf[Patch] :: patchClassesSoFar, tail)
        }
        case Nil => patchClassesSoFar
      }
    }

    //Create the list of patch classes
    val patches = createListOfPatchClasses(List(), patchClasses)
    patches.groupBy(_.patchName).foreach{ case (name, patches) =>
      assert(patches.size == 1, "Patch " + name + " used more than once")
    }
    patches
  }

  private def markPatchAsApplied(writer:DBWriter, patch: Patch) = {

    //Mark this patch as applied in the patch table
    writer.insert("SchemaEvolutionPatch", Map("patchNumber" -> patch.patchNumber,
                                          "patchName" -> patch.patchName,
                                          "patchDescription" -> patch.patchName,
                                          "dateApplied" -> new Timestamp))
  }

  private def applyPatch(starlingInit: StarlingInit, starling: RichDB, patch: Patch, context: PatchContext): Either[Option[String], Boolean] = {
    val deferredReason = patch.deferredReason(context)

    if (deferredReason.isEmpty) {
      Log.infoWithTime("SCHEMAEVOLUTION: About to apply patch [%d] with name [%s]" % (patch.patchNumber, patch.patchName)) {
        starling.inTransaction { writer => {
          patch.applyPatch(starlingInit, starling, writer)

          //Update the patch table to say this one is done
          markPatchAsApplied(writer, patch)
        } }
      }
      Right(patch.requiresRestart)
    } else {
      Left(deferredReason)
    }
  }


}