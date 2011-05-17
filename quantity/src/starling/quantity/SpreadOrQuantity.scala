package starling.quantity

case class SpreadOrQuantity(either: Either[Quantity, SpreadQuantity]) {
  override def toString = either match {
    case Left(q) => q.toString
    case Right(q) => q.toString
  }
}