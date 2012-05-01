println("\n ** Loading build-all...\n")

val versionNo = {
  //println("sys properties:")
  //System.getProperties.list(System.out)
  val v = Option(System.getProperty("version.number"))
  val ver = v.getOrElse("1.0-SNAPSHOT")
  println("\nVersion specified: " + v + ", version selected: " + ver + "\n")
  ver
}

:load maker/master.scala

import maker.task.TaskFailed
import maker.task.BuildResult
import maker.task.ProjectAndTask
import maker.task.Task
import maker.task.tasks._

def mkBuildResult(project : Project, task : Task) =
  BuildResult(Right("OK (faked)"), Set(), ProjectAndTask(project, task))

val buildResult = for {
  _ <- titanBuilder.clean
  _ <- { 
          titanBuilder.update
          // less than ideal, but ivy update is currently unreliable - so return a fake success always ultil the issue is sorted... 
          // (this is ok-ish because the local  cache is cummulative so even if first run fails it's usually ok by second or third pass
          BuildResult(Right("OK, checking disabled as Nexus is unreliable at the moment"), Set(), ProjectAndTask(titanBuilder, UpdateTask))
          mkBuildResult(titanBuilder, UpdateTask)
  }
  _ <- launcher.test

/**
 * invoicing generate some API code on the fly
 * it's quite static so we could check it into Git, or 
 *   unwind the actual command line from maven and call that directly
 */
//  _ <- titanInvoicing.mvn("compile", "-PWebService")  // currently problematic on teamcity agent
  
  _ <- titanBuilder.compile

// titan unit tests / classpath and deps needs more work before it's fully integrated with a single build
// so for now setup the titan components so the unit tests can be run, then run them explicitly

  _ <- {
         titanComponents.foreach(_.updateOnly("default", "test"))
         mkBuildResult(starlingTitanDeps, UpdateTask)
  }
  _ <- titanMurdoch.testOnly
  _ <- titanReferenceData.testOnly
  _ <- titanTradeService.testOnly
  _ <- titanLogistics.testOnly

  r <- starling publish(version = versionNo)
} yield r

// handle the build result to output a litle detail to console and return appropriate error codes for caller (i.e. for teamcity reporting etc)
buildResult.res match {
  case Right(result) => {
    println("Build OK")
    buildResult.stats.foreach(println)
    println(result)
    System.exit(0)
  }
  case Left(TaskFailed(_, reason)) => {
    println("Build Failed, reason: " + reason)
    println("Exiting with -1")
    System.exit(-1)
  }
}

