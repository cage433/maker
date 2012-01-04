package starling.daterange

import starling.utils.ImplicitConversions._
import starling.utils.StarlingEnum
import scalaz.Scalaz._

case class ObservationPoint(point:Option[(Day,ObservationTimeOfDay)]) {
  def this(day:Day) = this(Some((day, ObservationTimeOfDay.Default)))
  def day = point.map(_._1)
  def timeOfDay = point.map(_._2).getOrElse(ObservationTimeOfDay.RealTime)
  def timeShortName = timeOfDay.shortName
  def unparse = point.map { case (day,t) => day+"/"+t.shortName}.getOrElse("")
  def copyTime(time:Option[ObservationTimeOfDay]) = (time, point) match {
    case (Some(newTime), Some((day, _))) => ObservationPoint(Some(day, newTime))
    case _ => this
  }
  def copyDay(day: Day) = ObservationPoint(day, timeOfDay)
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

case class ObservationTimeOfDay private (name: String, shortName: String) extends Ordered[ObservationTimeOfDay] {
  def compare(that: ObservationTimeOfDay) = {
    val lhsIndex = ObservationTimeOfDay.sortIndex(that)
    val rhsIndex = ObservationTimeOfDay.sortIndex(this)
    if (lhsIndex == rhsIndex) {
      this.shortName.compare(that.shortName)
    } else {
      rhsIndex - lhsIndex
    }
  }
}

object ObservationTimeOfDay extends StarlingEnum(classOf[ObservationTimeOfDay], (o:ObservationTimeOfDay) => o.shortName, true) {
  def apply(name: String): ObservationTimeOfDay = ObservationTimeOfDay(name, name)
  def fromLongName(name: String): ObservationTimeOfDay = valuesByLongName(name)
  def fromLongName(name: Option[String]): Option[ObservationTimeOfDay] = name.map(fromLongName)
  def fromShortOrLongName(name: String): ObservationTimeOfDay = find(name) | fromLongName(name)
  def fromShortOrLongName(name: Option[String]): Option[ObservationTimeOfDay] = name.map(fromShortOrLongName)

  val Default = ObservationTimeOfDay("Default")
  val LMEClose = ObservationTimeOfDay("LME Close")
  val SHFEClose = ObservationTimeOfDay("SHFE Close")
  val COMEXClose = ObservationTimeOfDay("COMEX Close")
  val LondonClose = ObservationTimeOfDay("London Close")
  val CFETSPublicationTime = ObservationTimeOfDay("CFETS Publication Time")
  val LiborClose = ObservationTimeOfDay("Libor Close")
  val ECBPublicationTime = ObservationTimeOfDay("ECB Publication Time")

  val RealTime = ObservationTimeOfDay("Real Time")

  val LME_Official = new ObservationTimeOfDay("LME Official", "Official")
  val LME_Unofficial = new ObservationTimeOfDay("LME Unofficial", "Unofficial")
  val LME_AMR1 = new ObservationTimeOfDay("LME AMR1", "AMR1")
  val LME_PMR1 = new ObservationTimeOfDay("LME PMR1", "PMR1")

  private lazy val valuesByLongName = values.toMapWithKeys(_.name)
}