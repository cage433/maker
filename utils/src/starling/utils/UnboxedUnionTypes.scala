package starling.utils


object UnboxedUnionTypes {
  // http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/

  type ¬[A] = A => Nothing
  type ¬¬[A] = ¬[¬[A]]
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  /* e.g.

    def size[T : (Int |∨| String)#λ](t : T) = t match {
      case i : Int => i
      case s : String => s.length
    }
  */
}