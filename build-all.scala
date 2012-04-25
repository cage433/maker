println("\n ** Loading build-all...\n")

:load maker/master.scala

import maker.task.TaskFailed

val r = for {
  _ <- titanBuilder.clean
  _ <- titanBuilder.update
  _ <- launcher.test

/**
 * invoicing generate some code on the fly
* it's quite static so we could check it into Git, or 
*   unwind the actual command line from maven and call that directly
*/
//  _ <- titanInvoicing.mvn("compile", "-PWebService")
  r <- titanBuilder.compile
} yield r

r.res match {
  case Right(result) => {
    println("Build OK")
    r.stats.foreach(println)
    println(r)
    System.exit(0)
  }
  case Left(TaskFailed(_, reason)) => {
    println("Build Failed, reason: " + reason)
    println("Exiting with -1")
    System.exit(-1)
  }
}

