package starling.utils

import java.text.SimpleDateFormat
import java.util.Date

case class Stopwatch(name : String = ""){
  var startTime : Long = 0
  reset()
  
  def reset() : Long = {val savedMS = ms; startTime = currentTime(); savedMS}
  def offset(seconds: Long) = { startTime = startTime + (seconds * 1000000 * 1000); this }
  def currentTime() = System.nanoTime
  def nanos() = currentTime - startTime
  def ms() : Long = (nanos) / 1000000
  def s() : Long = ms / 1000
  def toStringSeconds = s() + "(s)"
  def asString = {
    if (name.nonEmpty) {
      name + "  " +Stopwatch.milliToHumanString(ms())
    } else {
      Stopwatch.milliToHumanString(ms())
    }
  }
  def asStringAndReset = {
    val s = asString
    reset()
    s
  }
  override def toString : String = asString
}

object Stopwatch {
  val global = new Stopwatch

  def time[A](name : String)(f: =>A) : A = {
    val stopwatch = new Stopwatch(name)
    val result = f
    println("Time: " + stopwatch)
    result
  }

  def time[A](f: =>A) : A = time("")(f)

  def milliToHumanString(milli:Long):String = {
    if (milli < 1000) {
      milli + "(ms)"
    } else if (milli < 60*1000) {
      (milli / 1000) + "(s) " + (milli%1000) + "(ms)"
    } else {
      (milli / (60*1000)) + "(m) " + ((milli/1000)%60) + "(s)"
    }
  }
  private val Format = new SimpleDateFormat("HH:mm.ss")
  def milliToTimeString(milli:Long) = {
    Format.format(new Date(milli))
  }

  def timeWithInfo[T](f: =>T): (TimingInfo, T) = {
    val stopWatch = new Stopwatch
    val result = f
    (TimingInfo(stopWatch.startTime, stopWatch.currentTime), result)
  }
}

case class TimingInfo(startTime:Long, endTime:Long) {
  def loggingLevel(thresholds: (Levels.Value, Int)*): Levels.Value = (Levels.Debug /: thresholds) {
    case (current, (level, threshold)) => if (timeTaken >= threshold && level < current) level else current
  }

  def loggingLevel(thresholds: Map[Levels.Value, Int]): Levels.Value = loggingLevel(thresholds.toSeq : _*)

  def loggingLevel(infoThreshold: Int, scale: Int = 10): Levels.Value =
    loggingLevel(Log.orderOfMagnitudeLoggingThresholds(infoThreshold, scale))

  val timeTaken = (endTime - startTime) / 1000000
  val timeTakenInMilliSeconds : Double = (endTime - startTime) / 1000000.0

  def slowest(other: TimingInfo): TimingInfo = if (timeTaken > other.timeTaken) this else other
}
