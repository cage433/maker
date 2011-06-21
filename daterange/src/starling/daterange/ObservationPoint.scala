package starling.daterange

import starling.utils.ImplicitConversions._
import starling.utils.{Named, StarlingEnum, OrderedComparable}

case class ObservationPoint(point:Option[(Day,ObservationTimeOfDay)]) {
  def this(day:Day) = this(Some((day, ObservationTimeOfDay.Default)))
  def day = point.map(_._1)
  def timeOfDay = point.map(_._2).getOrElse(ObservationTimeOfDay.RealTime)
  def timeName = timeOfDay.name
  def unparse = point.map { case (day,t) => day+"/"+t}.getOrElse("")
}

object ObservationPoint {
  val RealTime = new ObservationPoint(None)
  def apply(day:Day):ObservationPoint = new ObservationPoint(day)
  def apply(day:Day, time:ObservationTimeOfDay):ObservationPoint = new ObservationPoint(Some((day,time)))
  def fromExcel(d : Double) = ObservationPoint(Day.fromExcel(d))
  def parseText(s : String) = ObservationPoint(Day.parse(s))
  def parseExcel(s : String) = {
    val split = s.split("/")
    ObservationPoint(Day.quickParse(split(0)), ObservationTimeOfDay.fromName(split(1)))
  }
  def parse(point:Object) = point match {
    case null => ObservationPoint.RealTime
    case s:String if s.trim.isEmpty => ObservationPoint.RealTime
    case d:java.lang.Double => fromExcel(d.doubleValue)
    case s:String => parseExcel(s)
    case _ => throw new Exception("Expected day number or the format ddMMMyyyy/TimeOfDay")
  }
}

case class ObservationTimeOfDay(name:String) extends OrderedComparable[ObservationTimeOfDay] with Named {
  override def toString = name
  def compare(that: ObservationTimeOfDay) = {
    val lhsIndex = ObservationTimeOfDay.sortIndex(that)
    val rhsIndex = ObservationTimeOfDay.sortIndex(this)
    if (lhsIndex == rhsIndex) {
      this.name.compare(that.name)
    } else {
      rhsIndex - lhsIndex
    }
  }
}

object ObservationTimeOfDay extends StarlingEnum(classOf[ObservationTimeOfDay], true) {
  val Default = new ObservationTimeOfDay("Default")
  val LMEClose = new ObservationTimeOfDay("LME Close")
  val SHFEClose = new ObservationTimeOfDay("SHFE Close")
  val COMEXClose = new ObservationTimeOfDay("COMEX Close")
  val LondonClose = new ObservationTimeOfDay("London Close")
  val LiborClose = new ObservationTimeOfDay("Libor Close")
  val ECBPublicationTime = new ObservationTimeOfDay("ECB Publication Time")

  val RealTime = new ObservationTimeOfDay("Real Time")

  val Official = new ObservationTimeOfDay("Official")
  val Unofficial = new ObservationTimeOfDay("Unofficial")
  val AMR1 = new ObservationTimeOfDay("AMR1")
  val PMR1 = new ObservationTimeOfDay("PMR1")
}