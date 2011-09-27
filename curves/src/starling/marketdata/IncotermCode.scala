package starling.marketdata

import starling.utils.ImplicitConversions._

/**
 * For the moment Icoterm is just a placeholder, the logic for each type is given here:
 *  http://en.wikipedia.org/wiki/Incoterm
 */
case class IncotermCode(code: String)

object IncotermCode {
  val incoterms = List("CFR", "CIF", "DDP", "DES", "FOB", "ITT").map(IncotermCode(_))
  val incotermsLookup = incoterms.toMapWithKeys(_.code)

  def unapply(s: Any): Option[IncotermCode] = incotermsLookup.get(s.toString.toUpperCase)
}