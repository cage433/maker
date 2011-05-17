package starling.loopyxl

import LoopyXL._
import Response.Status._
import scala.collection.JavaConversions._
import java.lang.reflect.InvocationTargetException
import java.util.concurrent.atomic.AtomicInteger

import starling.utils.{ClosureUtil, ImplicitConversions, Log}
import ImplicitConversions._
import ClosureUtil._


class InvocationHandler(id: AtomicInteger, methodSource: MethodSource, authenticatedUser: => Closeable)
  extends TypedHandler[InvocationRequest, InvocationResponse](
  id, request => request.getInvocation, (builder, invocation) => builder.setInvocation(invocation)) {

  def handle(invocation: InvocationRequest) = invoke(invocation).mapFirst(InvocationResponse.newBuilder.setResult(_).build)

  private def invoke(invocation: InvocationRequest) = try {
    val lookup = methodSource.lookup(invocation.getMethodId)
    val parameterList: java.util.List[InvocationValue] = invocation.getParametersList

    using (authenticatedUser) { _ =>
      lookup.invoke(parameterList.toList) → SUCCESS
    }
  }
  catch {
    case e : InvocationTargetException => failure(e.getCause) → FAILURE
    case e => failure(e) → FAILURE
  }

  private def failure(throwable: Throwable) = Log.errorF("Failure", throwable) {
    InvocationValue.newBuilder.setType(InvocationValue.Type.STRING_VALUE)
      .setStringValue(throwable.getMessage ?? throwable.getClass.toString).build
  }
}
