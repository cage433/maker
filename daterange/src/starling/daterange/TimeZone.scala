package starling.daterange

import org.joda.time.DateTimeZone
import scalaz.Scalaz._


object TimeZone {
  val UTC = TimeZone("UTC")
  val GMT = TimeZone("GMT", Some("Etc/GMT"))
  val BST = TimeZone("BST", Some("Etc/GMT-1"))
  val Shanghai = TimeZone("Shanghai", Some("Asia/Shanghai"))
  val USEastern = TimeZone("US/Eastern")
  val USCentral = TimeZone("US/Central")
  val Tokyo = TimeZone("Asia/Tokyo")
}

case class TimeZone(name: String, id0: Option[String] = None) extends DateTimeZone(id0 | name) {
  val id = this.getID
  private val delegate = DateTimeZone.forID(id)

  def getNameKey(instant: Long) = delegate.getNameKey(instant)
  def getOffset(instant: Long) = delegate.getOffset(instant)
  def getStandardOffset(instant: Long) = delegate.getStandardOffset(instant)
  def isFixed = delegate.isFixed
  def nextTransition(instant: Long) = delegate.nextTransition(instant)
  def previousTransition(instant: Long) = delegate.previousTransition(instant)
  override def equals(other: Any) = delegate.equals(other)
}