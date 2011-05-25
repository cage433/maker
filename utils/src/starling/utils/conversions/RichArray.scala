package starling.utils.conversions


trait RichArray {
  implicit def enrichArray[A](array : Array[A]) = new RichArray[A](array)

  class RichArray[A](array : Array[A]) {
    def pair[B](f : A => B) : Array[(A,B)] = array.map(elem => (elem, f(elem)))
  }
}
