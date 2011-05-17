package starling.daterange


import java.util.concurrent.TimeUnit

class Duration(val time : Long, val timeUnit : TimeUnit) {
  def millis = timeUnit.toMillis(time)
}

case class Days(days: Long) extends Duration(days, TimeUnit.DAYS)
case class Hours(hours: Long) extends Duration(hours, TimeUnit.HOURS)
case class Minutes(minutes : Long) extends Duration(minutes, TimeUnit.MINUTES)
case class Seconds(seconds : Long) extends Duration(seconds, TimeUnit.SECONDS)
