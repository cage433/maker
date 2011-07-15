package starling.daterange

// TODO [22 Jan 2010] should be an enum?
object DayOfWeek {
  val List(monday, tuesday, wednesday, thursday, friday, saturday, sunday) =
    (0 until 7).toList.map(DayOfWeek(_))
  val daysOfWeek = Array("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  def parse(s: String) = DayOfWeek(daysOfWeek.indexWhere(d => s.toLowerCase.startsWith(d.toLowerCase)))
}

case class DayOfWeek(number : Int) {
  if (number < 0 || number > 6)
    throw new IllegalStateException("Invalid day of week number")

  def - (other: DayOfWeek) = number - other.number

  override def toString = DayOfWeek.daysOfWeek(number)
}
