println("\n ** Loading build-all...\n")

:load maker/master.scala

import maker.task.TaskFailed
import maker.task.BuildResult
import maker.task.ProjectAndTask
import maker.task.tasks._

val buildResult = for {
  _ <- titanBuilder.clean
  _ <- { 
          titanBuilder.update
          // less than ideal, but ivy update is currently unreliable - so return a fake success always ultil the issue is sorted... 
          // (this is ok-ish because the local  cache is cummulative so even if first run fails it's usually ok by second or third pass
          BuildResult(Right("OK, checking disabled as Nexus is unreliable at the moment"), Set(), ProjectAndTask(titanBuilder, UpdateTask))
  }
  _ <- launcher.test

/**
 * invoicing generate some API code on the fly
 * it's quite static so we could check it into Git, or 
 *   unwind the actual command line from maven and call that directly
 */
//  _ <- titanInvoicing.mvn("compile", "-PWebService")  // currently problematic on teamcity agent
  r <- titanBuilder.compile
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

