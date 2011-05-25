package starling.utils.conversions

import collection.mutable.ArraySeq


trait RichListOfArrays {
  implicit def ListOfArrayToRichListOfArray[T](input : List[Array[T]]) = new RichListOfArrays(input)

  class RichListOfArrays[E](input : List[Array[E]]) {
    def nullElementTo(value : E): List[ArraySeq[E]] = input.map(_.map {
      case null => value
      case a => a
    })
  }
}
