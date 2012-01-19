package starling.loopyxl

import java.lang.reflect.Method
import starling.utils.Log
import starling.utils.ImplicitConversions._
import collection.immutable.List


class MethodOwner(owner: AnyRef) {
  def dynamicMethods: List[DynamicMethod] = {
    val (valid, invalid) = annotatedMethods.map(new DynamicMethod(_, owner)).partition(_.isValid)

    invalid.foreach(logInvalid)

    valid
  }

  protected def logInvalid(method: DynamicMethod) {
    Log.error("ExcelMethod is not valid: %s, because it cannot contain classes: %s" %
      (method.name, method.unmarshallable.map(_.getSimpleName).mkString(", ")))
  }

  private def annotatedMethods: List[Method] = owner.getClass.getMethods.filter(hasAnnotation(_)).toList
  private def hasAnnotation(method: Method) = method.getAnnotation(classOf[ExcelMethod]) != null
}

object MethodOwner {
  implicit def lift(owners: Traversable[MethodOwner]) = new Lifted(owners)

  class Lifted(owners: Traversable[MethodOwner]) {
    def dynamicMethods : Traversable[DynamicMethod] = owners.flatMap(_.dynamicMethods)
  }
}