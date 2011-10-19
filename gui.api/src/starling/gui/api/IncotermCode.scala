package starling.gui.api

import starling.utils.ImplicitConversions._

/**
 * For the moment Icoterm is just a placeholder, the logic for each type is given here:
 *  http://en.wikipedia.org/wiki/Incoterm
 */
case class IncotermCode(code: String) {
  override def toString = code
}

case class ContractualLocationCode(code: String) {
  override def toString = code
}

case class GradeCode(code : String) {
  override def toString = code
}

object AreaCode{
  val EUR = AreaCode("EUR")
  val SAM = AreaCode("SAM")
  val NAM = AreaCode("NAM")
  val ASI = AreaCode("ASI")
  val CHN = AreaCode("CHN")
}
case class AreaCode(code:String) {
  override def toString = code
}