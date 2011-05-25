package starling.quantity

/**
 * implicit conversion to allow nicer syntax for dealing with quantities.
 *
 * if you import RichQuantity._ and UOM._ then you should be able to write
 * stuff like:
 *    1.0 (GBP) / 2.0 (USD) == 0.5 (GBP/USD)
 *
 */
case class RichQuantity(d : Double) {
  def apply(uom : UOM) : Quantity = Quantity(d, uom)
}

object RichQuantity {
  implicit def toRichQuantity(d : Double) = RichQuantity(d)
  implicit def toRichQuantity(i : Int) = RichQuantity(i)
}