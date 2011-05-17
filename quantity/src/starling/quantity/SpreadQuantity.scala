package starling.quantity
import starling.utils.ImplicitConversions._

case class SpreadQuantity(front: Quantity, back: Quantity) {
  assert(front.uom == back.uom, "UOMs need to be the same: " + (front, back))

  def uom = front.uom

  /**
   * full precision string of the value parts in the form d1/d2
   */
  def valueString = front.value + "/" + back.value

  private def format(value: Double, fmt : String = Quantity.FormatString) = value.format(fmt)

  override def toString = format(front.value) + "/" + format(back.value) + " " + front.uom
}

object SpreadQuantity {
  val Pattern = """([-\.\deE]+)[ ]*/[ ]*([-\.\deE]+)(.*)""".r

  def parse(s: String): Option[SpreadQuantity] = s match {
    case Pattern(f, b, u) => {
      val uom = UOM.fromString(u)
      Some(SpreadQuantity(Quantity(f.toDouble, uom), Quantity(b.toDouble, uom)))
    }
    case _ => None
  }
}