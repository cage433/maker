package starling.loopyxl

import java.lang.reflect.Method
import collection.immutable.List
import LoopyXL.InvocationValue
import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._


class DynamicMethod(method: Method, owner: AnyRef) {
  import ProtocolBuffers._

  private lazy val parameterTypes = method.getParameterTypes.toList
  private lazy val returnMarshaller = Marshallers.marshaller(method.getReturnType)

  def unmarshallable : List[Class[_]] = {
    if (parameterTypes.initOption.map(_.exists(isSequence)).getOrElse(false)) {
      parameterTypes.init.filter(isSequence)
    } else {
      Nil
    } ::: Marshallers.unmarshallable(method.getReturnType :: parameterTypes)
  }

  def isValid : Boolean = unmarshallable.isEmpty

  val id = method.hashCode
  val name = method.getName
  lazy val returnType = returnMarshaller.shortName
  lazy val parameterNames = parameterTypes.map(Marshallers.shortNames(_)).flatten

  def invoke(values: List[InvocationValue]) : InvocationValue = {
    val args: List[Object] = fromValues(values, method.getParameterTypes).asInstanceOf[List[Object]]

    decorate(iae => new IllegalArgumentException("Method: %s, args: %s" % (method.getName, args))) {
      returnMarshaller.serialize(method.invoke(owner, args: _*))
    }
  }

  private def isSequence(klazz: Class[_]): Boolean = classOf[Seq[_]].isAssignableFrom(klazz)
}