println("\n ** Loading build-all...\n")


:load maker/master.scala

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

/*
object Progn{
  def apply(closures : List[() => BuildResult] : BuildResult = {
    var result : BuildResult = null
    def recurse(closures : List[() => BuildResult]) : BuildResult = {
      closures match {
        case Nil => result
        case first::rest => {
          result = first()
          if (result.succeeded)
            recurse(rest)
          else
            result

        }
      }
    }
    recurse(closures)
  }
}
*/

val buildResults = for {
  _ <- titanBuilder.clean
  _ <- { 
          titanBuilder.update
          // less than ideal, but ivy update is currently unreliable - so return a fake success always ultil the issue is sorted... 
          // (this is ok-ish because the local  cache is cummulative so even if first run fails it's usually ok by second or third pass
          mkBuildResult(titanBuilder, UpdateTask)
  }
  i1 <- launcher.test
  _ <- doDocs(i1) //starlingDtoApi.docOnly(true) // build an aggregated doc of the starling api, unfortunately can't build whole docs as arg list is too big!

/**
 * invoicing generate some API code on the fly
 * it's quite static so we could check it into Git, or 
 *   unwind the actual command line from maven and call that directly
 */
//  _ <- titanInvoicing.mvn("compile", "-PWebService")  // currently problematic on teamcity agent

  _ <- { println("compiling titanBuilder"); titanBuilder.compile }

// titan unit tests / classpath and deps needs more work before it's fully integrated with a single build
// so for now setup the titan components so the unit tests can be run, then run them explicitly

  _ <- {
         titanComponents.foreach(_.updateOnly("default", "test"))
         mkBuildResult(starlingTitanDeps, UpdateTask)
  }
  _ <- { println("testing murdoch"); titanMurdoch.testOnly }
  _ <- { println("testing reference data"); titanReferenceData.testOnly }
  _ <- { println("testing trade service"); titanTradeService.testOnly }
  _ <- { println("testing invoicing"); titanInvoicing.testOnly }
  r <- { println("testing logistics"); titanLogistics.testOnly }

  // publish from maker, but only if this is a starling build and a version number was supplied!
//  r <- starling publish(version = versionNo.get) if (versionNo.isDefined && buildType == "starling")
} yield r

// only publish if we've a version number
val results = versionNo match {
  case Some(ver) if (buildType == "starling") => {
    println("publishing starling as version " + ver + "...")
    buildResults.flatMap(b => starling.publish(resolver = publishingResolverName, version = ver))
  }
  case None => buildResults
}

println("here, result = " + results)

// handle the build result to output a litle detail to console and return appropriate error codes for caller (i.e. for teamcity reporting etc)
if (results.succeeded) {
  println("Build OK:\n" + buildResults.result)
  println("task times : \n" + buildResults.taskCompletedTimes.map(t => (t._1, "took: " + t._2)).mkString("\n"))
  System.exit(0)
}
else {
  println("Build Failed, reason: \n" + results.result)
  println("Exiting with -1")
  System.exit(-1)
}

