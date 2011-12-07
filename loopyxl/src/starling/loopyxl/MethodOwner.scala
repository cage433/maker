package starling.loopyxl

import java.lang.reflect.Method
import scala.collection.JavaConversions._
import starling.utils.{VarLogger, Log}
import starling.utils.ImplicitConversions._

import collection.immutable.List


class MethodOwner(owner: AnyRef, log : VarLogger = Log) {
  def dynamicMethods : List[DynamicMethod] = {
    val methods = annotatedMethods.map(method => new DynamicMethod(method, owner));

    val unmarshallable = methods.toMapWithValues(_.unmarshallable).mapValues(_.distinct).filterValues(_.nonEmpty)
    unmarshallable.foreach(mc => log.error("ExcelMethod is not valid: %s, because it cannot contain classes: %s"
      % (mc._1.name, mc._2.map(_.getSimpleName).mkString(", "))))

    methods.filter(_.isValid)
  }

  private def annotatedMethods : List[Method] = owner.getClass.getMethods.filter(hasAnnotation(_)).toList
  private def hasAnnotation(method: Method) = method.getAnnotation(classOf[ExcelMethod]) != null
  private def onlyHasVarargsAtEnd(method: Method) = !method.getParameterTypes.init.exists(isSequence(_))
  private def logInvalid(method : DynamicMethod) = log.error("ExcelMethod is not valid: " + method.name)
  private def logInvalid(aClass : Class[_]) = log.error("ExcelMethod cannot contain class: " + aClass.getSimpleName)
  private def isSequence(aClass: Class[_]) = classOf[Seq[_]].isAssignableFrom(aClass)
}

object MethodOwner {
  implicit def lift(owners: Traversable[MethodOwner]) = new Lifted(owners)

  class Lifted(owners: Traversable[MethodOwner]) {
    def dynamicMethods : Traversable[DynamicMethod] = owners.flatMap(_.dynamicMethods)
  }
}