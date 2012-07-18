import Common._
import Utils._
import Starling._

import maker.task.TaskFailed
import maker.task.TaskSucceeded
import maker.task.DependencyTree
import maker.project._
import maker.project.TopLevelProject
import maker.project.ProjectLib
import maker.Props
import maker.utils.FileUtils._
import maker.utils.Log
import maker.utils.Log._
import maker.RichProperties._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.task.ProjectAndTask
import maker.task.Task
import maker.task.tasks._
import maker.utils._

import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._


// :load maker/Starling.scala
println("loading ci_starling_only build file (script)...")

val buildType = getPropertyOrDefault("build.type", "starling")
val versionNo = getProperty("version.number")
val publishingResolverName = getPropertyOrDefault("publishing.resolver", "starling-snapshot")

//maker.Maker.debug

def mkBuildResult(project : Project, task : Task) = {
  val pt = ProjectAndTask(project, task)
  BuildResult(List(TaskSucceeded(project, task, new Stopwatch)), DependencyTree[maker.task.ProjectAndTask](Map[maker.task.ProjectAndTask,Set[maker.task.ProjectAndTask]]()) , pt)
}
def doDocs(br : BuildResult) = {
  if (buildType == "starling") {
    println("generating documentation")
    br.flatMap(_ => starlingDTOApi.docOnly(true)) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!
  }
  else
    mkBuildResult(starlingDTOApi, DocTask)
}

println("finished loading definitions, starling build...")

val buildResults = for {
  _ <- starling.clean
  _ <- starling.update
  i1 <- launcher.test
  r <- doDocs(i1) //starlingDtoApi.docOnly(true) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!

} yield r

writeStarlingClasspath

// only publish if we've a version number
val results = versionNo match {
  case Some(ver) if (buildType == "starling") => {
    println("publishing starling as version " + ver + "...")
    buildResults.flatMap(b => starling.publish(resolver = publishingResolverName, version = ver))
  }
  case None => buildResults
}

println("build complete")
handleExit(results)
