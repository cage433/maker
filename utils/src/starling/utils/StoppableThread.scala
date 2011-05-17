package starling.utils

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.CountDownLatch
import ClosureUtil._


class StoppableThread(stoppableAction: (() => Boolean) => Unit) extends Stoppable {
  private val runLatch = new CountDownLatch(1)
  private val running = new AtomicBoolean(true)

  def start {
    startDaemon(() => { runLatch.countDown; stoppableAction(running.get) })

    runLatch.await
  }

  def stop = running.set(false)
}