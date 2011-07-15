package starling.daterange

/**
 * Time is split into start and end of day. At the start, options for that day are exercisable, and fixings
 *  may not have arrived. the reverse is true at the of day.
 */
object TimeOfDay {
  val StartOfDay = new TimeOfDay("Start of Day", "SoD")
  val EndOfDay = new TimeOfDay("End of Day", "EoD")
}

class TimeOfDay(name:String, val shortName:String) extends Serializable {
  override def equals(obj: Any): Boolean = obj match {
    case other: TimeOfDay => toString == other.toString
    case _ => false
  }
  override def hashCode = toString.hashCode
  override def toString = name
}
