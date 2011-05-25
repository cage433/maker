package starling.utils.log

import org.apache.log4j._

/**
 * User: brian
 * Date: 29-Dec-2009
 * Copyright (c) Trafigura 2009
 * Logger trait for logging per class / package
 */
trait Logger {
  lazy val Log = getLogger(this)

  /**
   * gets the required Log4j logger and wraps it. We store a map
   * of these per calling class to eliminate multiple copies
   */
  def getLogger(o: AnyRef): LogWrapper = {
    LogWrapper.wrappers.synchronized {
      LogWrapper.wrappers.getOrElseUpdate(o.getClass, new LogWrapper(o.getClass))
    }
  }
}

object LogWrapper {
  val wrappers: scala.collection.mutable.Map[Class[_], LogWrapper] = scala.collection.mutable.Map.empty
}

class LogWrapper(logger: org.apache.log4j.Logger) {

  def this(clazz : Class[_]) = this(org.apache.log4j.Logger.getLogger(clazz))

  def isTraceEnabled = logger.isTraceEnabled

  def trace(msg: => AnyRef) = if (isTraceEnabled) logger.trace(msg)

  def trace(msg: => AnyRef, t: => Throwable) = if (isTraceEnabled) logger.trace(msg, t)

  def assertLog(assertion: Boolean, msg: => String) = if (assertion) logger.assertLog(assertion, msg)

  def isDebugEnabled = logger.isDebugEnabled

  def debug(msg: => AnyRef) = if (isDebugEnabled) logger.debug(msg)

  def debug(msg: => AnyRef, t: => Throwable) = if (isDebugEnabled) logger.debug(msg, t)

  def isErrorEnabled = logger.isEnabledFor(Level.ERROR)

  def error(msg: => AnyRef) = if (isErrorEnabled) logger.error(msg)

  def error(msg: => AnyRef, t: => Throwable) = if (isErrorEnabled) logger.error(msg, t)

  def fatal(msg: AnyRef) = logger.fatal(msg)

  def fatal(msg: AnyRef, t: Throwable) = logger.fatal(msg, t)

  def name = logger.getName

  def isInfoEnabled = logger.isInfoEnabled

  def info(msg: => AnyRef) = if (isInfoEnabled) logger.info(msg)

  def info(msg: => AnyRef, t: => Throwable) = if (isInfoEnabled) logger.info(msg, t)

  def isEnabledFor(level: Priority) = logger.isEnabledFor(level)

  def isWarnEnabled = isEnabledFor(Level.WARN)

  def warn(msg: => AnyRef) = if (isWarnEnabled) logger.warn(msg)

  def warn(msg: => AnyRef, t: => Throwable) = if (isWarnEnabled) logger.warn(msg, t)
}
