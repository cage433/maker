package starling.utils

object StoppableThreadTests {
  def main(args: Array[String]) {
    val thread = new StoppableThread(isRunning => new Runnable {
      def run() {
        println("Running")
        while (isRunning()) {
          println(".")

          Thread.sleep(500)
        }
      }
    })

    println("Starting")

    thread.start

    Thread.sleep(5000)

    println("Stopping")

    thread.stop

    println("Stopped")
  }
}