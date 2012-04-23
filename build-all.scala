println("\n ** Loading build-all...\n")

:load maker/master.scala

import maker.task.TaskFailed

titanBuilder.clean
titanBuilder.update
val r = titanBuilder.compile

r.res match {
  case Right(_) => {
    println("Build OK")
    r.stats.foreach(println)
    System.exit(0)
  }
  case Left(TaskFailed(_, reason)) => {
    println("Build Failed, reason: " + reason)
    System.exit(-1)
  }
}

