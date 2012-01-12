package starling.utils

import org.apache.log4j._

import starling.utils.ImplicitConversions._
import util.DynamicVariable
import scalaz.Scalaz._
import starling.manager.Profiler


/**
 * A thin wrapper around log4j.
 */
object Log extends VarLogger {
  private val rootLogger = Log4JLogger.logger

  def forName(name: String): VarLogger = Log4JLogger.forName(name)
  def forClass[T: Manifest]: VarLogger = Log4JLogger.forClass(implicitly[Manifest[T]].erasure)

  /** Returns logging thresholds for Info, Warn, and Error */
  def orderOfMagnitudeLoggingThresholds(infoThreshold: Int, scale: Int = 10) = Map(
    Levels.Info → infoThreshold, Levels.Warn → (infoThreshold * scale), Levels.Error → (infoThreshold * scale * scale))

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
  override def level[T](newLevel: Levels.Value)(thunk: => T) = rootLogger.level(newLevel)(thunk)
  override def name = rootLogger.name
  override def isInfoEnabled = rootLogger.isInfoEnabled
  override def info(msg: => AnyRef) = rootLogger.info(msg)
  override def info(msg: => AnyRef, t: => Throwable) = rootLogger.info(msg, t)
  override def isWarnEnabled = rootLogger.isWarnEnabled
  override def warn(msg: => AnyRef) = rootLogger.warn(msg)
  override def warn(msg: => AnyRef, t: => Throwable) = rootLogger.warn(msg, t)
  override def isTraceEnabled = rootLogger.isTraceEnabled
}

trait Log {
  lazy val log: VarLogger = Log4JLogger.forClass(getClass)
}

trait VarLogger {
  def infoWithTime[T](message: String)(f: => T): T = withTime(message, msg => info(msg), f)
  def debugWithTime[T](message: String)(f: => T): T = withTime(message, msg => debug(msg), f)

  def debugWithTimeGapTop[T](message:String)(f: =>T) = {
    println("")
    println("")
    debugWithTime(message){f}
  }
  def debugWithTimeGapBottom[T](message:String)(f: =>T) = {
    val r = debugWithTime(message){f}
    println("")
    println("")
    r
  }

  def withName[T](name: String)(f: => T): T = ThreadUtils.withNamedThread(name)(f)

  protected def withTime[T](message: String, logger: AnyRef => Unit, f: => T) = {
    val stopwatch = new Stopwatch()

    withName(message) {
      Profiler.time(message) {
        logger(message + " Start")
        val result = f;
        logger(message + " Complete. Time: " + stopwatch)
        result
      }
    }
  }

  /**
   * Logs a message at a dynamically determined logging level
   *
   * {@link starling.utils.Log#orderOfMagnitudeLoggingThresholds}
   */
  def logWithTime[T](message: String, infoThreshold: Int, scale: Int = 10)(f: => T): T = {
    logWithTime(message, Log.orderOfMagnitudeLoggingThresholds(infoThreshold, scale))(f)
  }

  /**
   * Logs a message at a dynamically determined logging level
   */
  def logWithTime[T](message: String, loggingThresholds: Map[Levels.Value, Int])(f: => T): T = {
    val (timing, result) = Stopwatch.timeWithInfo(f)

    log(timing.loggingLevel(loggingThresholds), message.format(timing.timeTaken))

    result
  }

  def debugF[T](msg: => AnyRef)(f: => T): T                  = {debug(msg); f}
  def infoF[T](msg: => AnyRef)(f: => T): T                   = {info(msg); f}
  def infoF[T](msg: => AnyRef, t: => Throwable)(f: => T): T  = {info(msg, t); f}
  def warnF[T](msg: => AnyRef)(f: => T): T                   = {warn(msg); f}
  def warnF[T](msg: => AnyRef, t: => Throwable)(f: => T): T  = {warn(msg, t); f}
  def errorF[T](msg: => AnyRef)(f: => T): T                  = {error(msg); f}
  def errorF[T](msg: => AnyRef, t: => Throwable)(f: => T): T = {error(msg, t); f}
  def never(msg: => AnyRef) {}
  def neverF(msg: => AnyRef) {}
  def never(msg: => AnyRef, t: => Throwable) {}
  def logException[T](msg: String = "")(action: => T): Either[Throwable, T] = {
    ClosureUtil.safely { action }.update(t => error(msg, t), identity)
  }


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

  def level[T](newLevel: Levels.Value)(thunk: => T): T = thunk
  def off[T](thunk: => T): T = level(Levels.Off) { thunk }

  def name: String = "Null"
  // def parent = logger.getParent

  def isInfoEnabled: Boolean = false

  def info(msg: => AnyRef): Unit = ()

  def info(msg: => AnyRef, t: => Throwable): Unit = ()

  def isEnabledFor(level: Levels.Value): Boolean = false

  def isWarnEnabled: Boolean = false

  def warn(msg: => AnyRef): Unit = ()

  def warn(msg: => AnyRef, t: => Throwable): Unit = ()

  def log(level: Levels.Value, msg: => AnyRef): Unit = level match {
    case Levels.Trace => trace(msg)
    case Levels.Debug => debug(msg)
    case Levels.Info => info(msg)
    case Levels.Warn => warn(msg)
    case Levels.Error => error(msg)
    case Levels.Fatal => fatal(msg)
    case _ =>
  }
}

object Levels extends Enumeration {
  val All = Value(15, "All")
  val Trace = Value(13, "Trace")
  val Debug = Value(11, "Debug")
  val Info = Value(9, "Info")
  val Warn = Value(7, "Warn")
  val Error = Value(5, "Error")
  val Fatal = Value(3, "Fatal")
  val Off = Value(1, "Off")
}

object Log4JLogger {
  //System.setProperty("log4j.configuration", "utils/resources/log4j.properties")

  lazy val logger: VarLogger = new Log4JLogger(Logger.getRootLogger, levelTransformer)
  def forName(name: String): VarLogger = new Log4JLogger(Logger.getLogger(name), levelTransformer)
  def forClass(clazz: Class[_]): VarLogger = new Log4JLogger(Logger.getLogger(clazz), levelTransformer)

  private val levelTransformer = new DynamicVariable[(Levels.Value) => Levels.Value](identity _)

  val isOsgi = false
//    try { classOf[Category].getClass.getMethod("getLevel"); false }
//    catch { case _:NoSuchMethodException => true }
}

class Log4JLogger(val logger: Logger, levelTransformer: DynamicVariable[(Levels.Value) => Levels.Value]) extends VarLogger {
  override def isTraceEnabled = isEnabledFor(Levels.Trace)

  override def trace(msg: => AnyRef) = if (isTraceEnabled) logger.trace(msg)

  override def trace(msg: => AnyRef, t: => Throwable) = if (isTraceEnabled) logger.trace(msg, t)

  override def assertLog(assertion: Boolean, msg: => String) = if (assertion) logger.assertLog(assertion, msg)

  override def isDebugEnabled = isEnabledFor(Levels.Debug)

  override def debug(msg: => AnyRef) = if (isDebugEnabled) logger.debug(msg)

  override def debug(msg: => AnyRef, t: => Throwable) = if (isDebugEnabled) logger.debug(msg, t)

  override def isErrorEnabled = isEnabledFor(Levels.Error)

  override def error(msg: => AnyRef) = if (isErrorEnabled) logger.error(msg)

  override def error(msg: => AnyRef, t: => Throwable) = if (isErrorEnabled) logger.error(msg, t)

  override def fatal(msg: AnyRef) = logger.fatal(msg)

  override def fatal(msg: AnyRef, t: Throwable) = logger.fatal(msg, t)

  private def getInheritedLevel: Level = {
    if (Log4JLogger.isOsgi) {
      Level.INFO
    } else {
      def recurse(category: Category): Level = {
        category.getLevel ?? recurse(category.getParent)
      }

      recurse(logger)
    }
  }

  override def level = levelTransformer.value(getInheritedLevel match {
    case Level.ALL => Levels.All
    case Level.DEBUG => Levels.Debug
    case Level.ERROR => Levels.Error
    case Level.WARN => Levels.Warn
    case Level.FATAL => Levels.Fatal
    case Level.INFO => Levels.Info
    case Level.TRACE => Levels.Trace
    case Level.OFF => Levels.Off
    case _ => Levels.Off
  })

  val liftToLog4J: PartialFunction[Levels.Value, Level] = {
    case Levels.All => Level.ALL
    case Levels.Debug => Level.DEBUG
    case Levels.Error => Level.ERROR
    case Levels.Warn => Level.WARN
    case Levels.Fatal => Level.FATAL
    case Levels.Info => Level.INFO
    case Levels.Trace => Level.TRACE
    case Levels.Off => Level.OFF
    case _ => Level.OFF
  }

  override def isEnabledFor(level: Levels.Value): Boolean = this.level >= level

  override def level_=(level: Levels.Value) = logger.setLevel(liftToLog4J(level))

  override def level[T](newLevel: Levels.Value)(thunk: => T) = levelTransformer.withValue(_ => newLevel) { thunk }

  override def name = logger.getName

  override def isInfoEnabled = isEnabledFor(Levels.Info)

  override def info(msg: => AnyRef) = {
    if (msg.toString.trim == "") throw new Exception("Don't log blank lines")
    if (isInfoEnabled) logger.info(msg)
  }

  override def info(msg: => AnyRef, t: => Throwable) = if (isInfoEnabled) logger.info(msg, t)

  def isEnabledFor(level: Priority) = logger.isEnabledFor(level)

  override def isWarnEnabled = isEnabledFor(Levels.Warn)

  override def warn(msg: => AnyRef) = if (isWarnEnabled) logger.warn(msg)

  override def warn(msg: => AnyRef, t: => Throwable) = if (isWarnEnabled) logger.warn(msg, t)
}


