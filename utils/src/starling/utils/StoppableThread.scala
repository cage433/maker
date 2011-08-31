package starling.utils

import java.util.concurrent.CountDownLatch
import ClosureUtil._


class StoppableThread(stoppableAction: (() => Boolean) => Unit) extends Stopable {
  private val runLatch = new CountDownLatch(1)

  override def start {
    super.start

    startDaemon(() => { runLatch.countDown; stoppableAction(running.get) })

    runLatch.await
  }
}