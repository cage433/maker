package starling.utils

import conversions._


object ImplicitConversions extends RichAnys
  with RichArray
  with RichBoolean
  with RichCollection
  with RichDateTime
  with RichDouble
  with RichEither
  with RichExecutor
  with RichFunction
  with RichList
  with RichListOfArrays
  with RichLocalTime
  with RichManifest
  with RichMaps
  with RichOption
  with RichOrdering
  with RichRunnable
  with RichThreadLocal
  with RichThrowables
  with RichTraversables
  with RichTuple
  with RichSeq
  with RichString
  with CollectionsSyntacticSugar
  with CompInt
  with CompLong {

  def notNull[A](a: A)(implicit ma: Manifest[A]) = ma.requireNotNull(a)
}
