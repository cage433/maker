package starling.utils

/**
 * Created by IntelliJ IDEA.
 * User: davem
 * Date: 03/08/11
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */

trait TestTimer {

  import java.lang.System.{currentTimeMillis => currentTime}

  private var start: Long = _
  private var last: Long = _
  private var name: Option[String] = None

  def startTestTimer(name: String): Unit = {
    start = currentTime
    last = start
    this.name = Some(getClass.getSimpleName + ": " + name)
    logTestTime("Started test timer")
  }

  def logTestTime(name: String): Unit = {
    val now = currentTime
    val dt = now - start
    val ddt = now - last
    last = now
    log(name, dt, ddt)
  }

  private def log(name: String, t: Long, dt: Long): Unit = println("[timing] " + toString + ": " + name + " @ " + t + " ms (" + dt + " ms)")

  override def toString(): String = name.getOrElse(getClass.getSimpleName)
}