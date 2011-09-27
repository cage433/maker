package starling.utils.conversions

import starling.utils.ImplicitConversions._


trait RichManifest {
  implicit def enrichManifest[T](manifest : Manifest[T]) = new RichManifest(manifest)

  class RichManifest[T](manifest : Manifest[T]) {
    def isInstance(any: Any) = manifest.erasure.isInstance(any)

    def cast(any: Any): T = safeCast(any).getOrElse(
      throw new Exception("%s [%s] is not of type %s" % (any, getClassOf(any), manifest.erasure)))

    def safeCast(any: Any): Option[T] = if (any != null && isInstance(any)) Some(any.asInstanceOf[T]) else None
    def castAll(list: List[Any], error: Any => T): List[T] = list.map(value => safeCast(value).getOrElse(error(value)))
    def requireNotNull(value: T) = {
      val n = nullTypes(value)
      if (!n.isEmpty) {
        throw new Exception("Unexpected null, type(s): " + n.mkString(","))
      }
    }

    private def getClassOf(any: Any): String = if (any.isInstanceOf[AnyRef]) any.asInstanceOf[AnyRef].getClass.toString else "?"

    private def nullTypes(value: T): List[String] = value match {
      case product: Product => {
        val nullElements = product.productIterator.toList.indexesMatching(_ == null)
        manifest.typeArguments.elementsAt(nullElements).map(_.toString)
      }
      case _ => if (value == null) List(manifest.toString) else Nil
    }
  }
}