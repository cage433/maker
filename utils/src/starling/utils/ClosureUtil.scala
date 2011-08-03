package starling.utils

import scala.util.control.Exception._


object ClosureUtil {
  import ImplicitConversions._

  type Closeable = { def close(); }
  val Null = closable({})
  def closable(action: => Unit) = new { def close() {action} }

  def using[R <: Closeable, T](resource: R)(body: R => T): T = try {
    body(resource)
  } finally {
    resource.close
  }

  def logException[T](msg: String = "")(action: => T) = safely { action }.update(t => Log.error(msg, t), identity)
  def decorate[T](message: => String)(action: => T): T = decorate(e => throw new Exception(message, e))(action)
  def decorate[T](f: Exception => Exception)(action: => T): T = try { action } catch { case e: Exception => throw f(e) }
  def safely[T](action: => T): Either[Throwable, T] = catching(classOf[Exception]) either(action)
  def forever(action: => Unit): Unit = while (true) action
  def daemon(action: () => Unit): Thread = daemon(runnable(action))
  def daemon(runnable: Runnable): Thread = new Thread(runnable).update(_.setDaemon(true))
  def startDaemon(action : () => Unit): Unit = daemon(action).start
}