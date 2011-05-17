package starling.instrument.physical

/**
 * For the moment Icoterm is just a placeholder, the logic for each type is given here:
 *  http://en.wikipedia.org/wiki/Incoterm
 */
case class Incoterm(name: String)

object Incoterm {
  val incoterms = List("CFR", "CIF", "DDP", "DES", "FOB", "ITT").map(s => Incoterm(s))
  val incotermsLookup = incoterms.map(i => i.name -> i).toMap

  def unapply(s: Any): Option[Incoterm] = incotermsLookup.get(s.toString.toUpperCase)
}