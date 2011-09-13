package starling.loopyxl

import starling.utils.ImplicitConversions._
import LoopyXL._
import Response.Status._
import java.util.concurrent.atomic.AtomicInteger
import starling.utils.Log


class LookupHandler(id: AtomicInteger, methodSource: MethodSource) extends TypedHandler[LookupRequest, LookupResponse](
  id, request => request.getLookup, (builder, lookup) => builder.setLookup(lookup)) with Log {

  def handle(lookup : LookupRequest) = LookupResponse.newBuilder
    .update { lookupResponse => methodSource.getMethods.foreach(method => lookupResponse.addDefinitions(methodDefinition(method))) }
    .build → SUCCESS

  private def methodDefinition(method: DynamicMethod) = log.infoF("DynamicMethod: " + method.name) {
    MethodDefinition.newBuilder
      .setId(method.id)
      .setName(method.name)
      .setReturnType(method.returnType)
      .update { methodDefinition => method.parameterNames.foreach(name => methodDefinition.addParameterTypes(name)) }
  }
}