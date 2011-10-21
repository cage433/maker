package starling.utils

import java.util.concurrent.ThreadFactory

class NamedDaemonThreadFactory(name: String) extends ThreadFactory {
  val sequence = new java.util.concurrent.atomic.AtomicInteger(0)

  def newThread(r: Runnable) = {
    val t = new Thread(r)
    t.setDaemon(true)
    t.setName(sequence.getAndIncrement + " " + name)
    t
  }
}