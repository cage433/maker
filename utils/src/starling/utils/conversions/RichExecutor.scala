package starling.utils.conversions

import java.util.concurrent.Executor


trait RichExecutor {
  implicit def enrichExecutor(executor : Executor) = new RichExecutor(executor)

  class RichExecutor(executor : Executor) {
    def execute(action : Unit) = executor.execute(new Runnable { def run = action} )
  }
}