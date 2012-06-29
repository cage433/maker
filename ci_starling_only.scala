println("\n ** Loading Starling build...\n")


import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.task.TaskFailed
import maker.task.TaskSucceeded
import maker.task.DependencyTree

:load maker/Starling.scala

val buildType = getPropertyOrDefault("build.type", "starling")
val versionNo = getProperty("version.number")
val publishingResolverName = getPropertyOrDefault("publishing.resolver", "starling-snapshot")

import maker.task.TaskFailed
import maker.task.BuildResult
import maker.task.ProjectAndTask
import maker.task.Task
import maker.task.tasks._
import maker.utils._

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
  _ <- { 
          starling.update
          // less than ideal, but ivy update is currently unreliable - so return a fake success always ultil the issue is sorted... 
          // (this is ok-ish because the local  cache is cummulative so even if first run fails it's usually ok by second or third pass
          mkBuildResult(starling, UpdateTask)
  }
  i1 <- launcher.test
  r <- doDocs(i1) //starlingDtoApi.docOnly(true) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!

} yield r

// only publish if we've a version number
val results = versionNo match {
  case Some(ver) if (buildType == "starling") => {
    println("publishing starling as version " + ver + "...")
    buildResults.flatMap(b => starling.publish(resolver = publishingResolverName, version = ver))
  }
  case None => buildResults
}

handleExit(results)

println("build complete")

