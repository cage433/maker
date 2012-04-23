println("\n ** Loading build-all...\n")

:load maker/master.scala

import maker.task.TaskFailed

titanBuilder.clean
titanBuilder.update

/**
 * invoicing generate some code on the fly
* it's quite static so we could check it into Git, or 
*   unwind the actual command line from maven and call that directly
*/
//titanInvoicing.mvn("compile", "-PWebService")

val r = titanBuilder.compile

r.res match {
  case Right(result) => {
    println("Build OK")
    r.stats.foreach(println)
    println(r)
    System.exit(0)
  }
  case Left(TaskFailed(_, reason)) => {
    println("Build Failed, reason: " + reason)
    System.exit(-1)
  }
}

