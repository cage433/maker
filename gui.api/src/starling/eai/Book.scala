package starling.eai

case class Book(bookID: Int)

object Book {
  val LondonDerivatives = Book(43)
  val GasolineSpec = Book(149)
  val LondonDerivativesOptions = Book(173)
  val CrudeSpecNorthSea = Book(197)
  val HoustonDerivatives = Book(190)
  val all = List(LondonDerivatives, GasolineSpec, LondonDerivativesOptions, CrudeSpecNorthSea, HoustonDerivatives)
}