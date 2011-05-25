package starling.utils

object ClosureUtil {
  import ImplicitConversions._

  type Closeable = { def close(); }
  val Null = closable({})
  def closable(action: => Unit) = new { def close() {action} }

  case class Bound[R <: Closeable](resource : R, bindings : Closeable *) {
    def close() {
      resource.close
      bindings.foreach(_.close)
    }
  }

  def using[R <: Closeable, T](resource : R)(body : R => T) : T = try {
    body(resource)
  } finally {
    resource.close
  }

  def using[R <: Closeable, T](boundResource : Bound[R])(body : R => T) : T = try {
    body(boundResource.resource)
  } finally {
    boundResource.close
  }

  def logException[T](action: => T): T = decorate(e => {e.printStackTrace; e})(action)
  def decorate[T](message: => String)(action: => T): T = decorate(e => throw new Exception(message, e))(action)
  def decorate[T](f: Exception => Exception)(action: => T): T = try { action } catch { case e: Exception => throw f(e) }
  def safely[T](action: => T): Either[Exception, T] = try { Right(action) } catch { case e : Exception => Left(e) }
  def forever(action: => Unit): Unit = while (true) action
  def daemon(action: () => Unit): Thread = daemon(runnable(action))
  def daemon(runnable: Runnable): Thread = new Thread(runnable).update(_.setDaemon(true))
  def startDaemon(action : () => Unit): Unit = daemon(action).start
}