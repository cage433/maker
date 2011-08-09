package starling.utils

import org.apache.log4j._

import starling.utils.ImplicitConversions._


class AdaptingLogger(val rootLogger: VarLogger) extends VarLogger {
  override def trace(msg: => AnyRef) = rootLogger.trace(msg)
  override def trace(msg: => AnyRef, t: => Throwable) = rootLogger.trace(msg, t)
  override def assertLog(assertion: Boolean, msg: => String) = rootLogger.assertLog(assertion, msg)
  override def isEnabledFor(level: Levels.Value) = rootLogger.isEnabledFor(level)
  override def isDebugEnabled = rootLogger.isDebugEnabled
  override def debug(msg: => AnyRef) = rootLogger.debug(msg)
  override def debug(msg: => AnyRef, t: => Throwable) = rootLogger.debug(msg, t)
  override def isErrorEnabled = rootLogger.isEnabledFor(Levels.Error)
  override def error(msg: => AnyRef) = rootLogger.error(msg)
  override def error(msg: => AnyRef, t: => Throwable) = rootLogger.error(msg, t)
  override def fatal(msg: AnyRef) = rootLogger.fatal(msg)
  override def fatal(msg: AnyRef, t: Throwable) = rootLogger.fatal(msg, t)
  override def level = rootLogger.level
  override def level_=(level: Levels.Value) = rootLogger.level = level
  override def name = rootLogger.name
  override def isInfoEnabled = rootLogger.isInfoEnabled
  override def info(msg: => AnyRef) = rootLogger.info(msg)
  override def info(msg: => AnyRef, t: => Throwable) = rootLogger.info(msg, t)
  override def isWarnEnabled = rootLogger.isWarnEnabled
  override def warn(msg: => AnyRef) = rootLogger.warn(msg)
  override def warn(msg: => AnyRef, t: => Throwable) = rootLogger.warn(msg, t)
  override def isTraceEnabled = rootLogger.isTraceEnabled
}

/**
 * A thin wrapper around log4j.
 */
object Log extends ExtendedLog(Log4JLogger.logger) {
  def forName(name: String)     = new ExtendedLog(Log4JLogger.forName(name))
  def forClass[T: Manifest]     = new ExtendedLog(Log4JLogger.forClass(implicitly[Manifest[T]].erasure))
}

trait Log {
  lazy val log = new ExtendedLog(Log4JLogger.forClass(getClass))
}

class ExtendedLog(adapted: VarLogger) extends AdaptingLogger(adapted) {
  def infoWithTime[T](message:String)(f: =>T) = {
    val stopwatch = new Stopwatch()
    val oldThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(oldThreadName + " > " + message)
      info(message + " Start")
      val result = f;
      println (message + " Complete. Time: " + stopwatch)
      result
    } finally {
      Thread.currentThread.setName(oldThreadName)
    }
  }
  def infoWithTimeGapTop[T](message:String)(f: =>T) = {
    println("")
    println("")
    infoWithTime(message){f}
  }
  def infoWithTimeGapBottom[T](message:String)(f: =>T) = {
    val r = infoWithTime(message){f}
    println("")
    println("")
    r
  }

  def infoF[T](msg: => AnyRef)(f: => T)                   = {info(msg); f}
  def infoF[T](msg: => AnyRef, t: => Throwable)(f: => T)  = {info(msg, t); f}
  def warnF[T](msg: => AnyRef)(f: => T)                   = {warn(msg); f}
  def warnF[T](msg: => AnyRef, t: => Throwable)(f: => T)  = {warn(msg, t); f}
  def errorF[T](msg: => AnyRef)(f: => T)                  = {error(msg); f}
  def errorF[T](msg: => AnyRef, t: => Throwable)(f: => T) = {error(msg, t); f}
  def never(msg: => AnyRef) {}
  def neverF(msg: => AnyRef) {}
  def never(msg: => AnyRef, t: => Throwable) {}
  def logException[T](msg: String = "")(action: => T) = ClosureUtil.safely { action }.update(t => error(msg, t), identity)
}

trait VarLogger {
  def isTraceEnabled: Boolean = false

  def trace(msg: => AnyRef): Unit = ()

  def trace(msg: => AnyRef, t: => Throwable): Unit = ()

  def assertLog(assertion: Boolean, msg: => String): Unit = ()

  def isDebugEnabled: Boolean = false

  def debug(msg: => AnyRef): Unit = ()

  def debug(msg: => AnyRef, t: => Throwable): Unit = ()

  def isErrorEnabled: Boolean = false

  def error(msg: => AnyRef): Unit = ()

  def error(msg: => AnyRef, t: => Throwable): Unit = ()

  def fatal(msg: AnyRef): Unit = ()

  def fatal(msg: AnyRef, t: Throwable): Unit = ()

  def level: Levels.Value = Levels.Off

  def level_=(level: Levels.Value): Unit = ()

  def name: String = "Null"
  // def parent = logger.getParent

  def isInfoEnabled: Boolean = false

  def info(msg: => AnyRef): Unit = ()

  def info(msg: => AnyRef, t: => Throwable): Unit = ()

  def isEnabledFor(level: Levels.Value): Boolean = false

  def isWarnEnabled: Boolean = false

  def warn(msg: => AnyRef): Unit = ()

  def warn(msg: => AnyRef, t: => Throwable): Unit = ()
}

object Levels extends Enumeration {
  val All = Value(1, "All")
  val Trace = Value(3, "Trace")
  val Debug = Value(5, "Debug")
  val Warn = Value(7, "Warn")
  val Error = Value(9, "Error")
  val Fatal = Value(11, "Fatal")
  val Info = Value(13, "Info")
  val Off = Value(15, "Off")
}

object Log4JLogger {
  System.setProperty("log4j.configuration", "utils/resources/log4j.properties")

  lazy val logger = new Log4JLogger(Logger.getRootLogger)
  def forName(name: String) = new Log4JLogger(Logger.getLogger(name))
  def forClass(clazz: Class[_]) = new Log4JLogger(Logger.getLogger(clazz))
}

class Log4JLogger(val logger: Logger) extends VarLogger {
  override def isTraceEnabled = logger.isTraceEnabled

  override def trace(msg: => AnyRef) = if (isTraceEnabled) logger.trace(msg)

  override def trace(msg: => AnyRef, t: => Throwable) = if (isTraceEnabled) logger.trace(msg, t)

  override def assertLog(assertion: Boolean, msg: => String) = if (assertion) logger.assertLog(assertion, msg)

  override def isDebugEnabled = logger.isDebugEnabled

  override def debug(msg: => AnyRef) = if (isDebugEnabled) logger.debug(msg)

  override def debug(msg: => AnyRef, t: => Throwable) = if (isDebugEnabled) logger.debug(msg, t)

  override def isErrorEnabled = logger.isEnabledFor(Level.ERROR)

  override def error(msg: => AnyRef) = if (isErrorEnabled) logger.error(msg)

  override def error(msg: => AnyRef, t: => Throwable) = if (isErrorEnabled) logger.error(msg, t)

  override def fatal(msg: AnyRef) = logger.fatal(msg)

  override def fatal(msg: AnyRef, t: Throwable) = logger.fatal(msg, t)

  override def level = logger.getLevel match {
    case Level.ALL => Levels.All
    case Level.DEBUG => Levels.Debug                 //  val height = 500
//  chooser.preferredSize = new Dimension(70,height)

    case Level.ERROR => Levels.Error
    case Level.WARN => Levels.Warn
    case Level.FATAL => Levels.Fatal
    case Level.INFO => Levels.Info
    case Level.TRACE => Levels.Trace
    case Level.OFF => Levels.Off
  }

  val liftToLog4J: PartialFunction[Levels.Value, Level] = {
    case Levels.All => Level.ALL
    case Levels.Debug => Level.DEBUG
    case Levels.Error => Level.ERROR
    case Levels.Warn => Level.WARN
    case Levels.Fatal => Level.FATAL
    case Levels.Info => Level.INFO
    case Levels.Trace => Level.TRACE
    case Levels.Off => Level.OFF
  }

  override def isEnabledFor(level: Levels.Value): Boolean = logger.isEnabledFor(liftToLog4J(level))

  override def level_=(level: Levels.Value) = logger.setLevel(liftToLog4J(level))

  override def name = logger.getName

  override def isInfoEnabled = logger.isInfoEnabled

  override def info(msg: => AnyRef) = if (isInfoEnabled) logger.info(msg)

  override def info(msg: => AnyRef, t: => Throwable) = if (isInfoEnabled) logger.info(msg, t)

  def isEnabledFor(level: Priority) = logger.isEnabledFor(level)

  override def isWarnEnabled = isEnabledFor(Level.WARN)

  override def warn(msg: => AnyRef) = if (isWarnEnabled) logger.warn(msg)

  override def warn(msg: => AnyRef, t: => Throwable) = if (isWarnEnabled) logger.warn(msg, t)
}


