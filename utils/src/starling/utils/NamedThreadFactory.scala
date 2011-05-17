package starling.utils

import java.util.concurrent.ThreadFactory

case class NamedThreadFactory(name: String) extends ThreadFactory {
  def newThread(r: Runnable) = {
    new Thread(r, name)
  }
}
