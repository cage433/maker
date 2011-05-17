package starling.loopyxl

import LoopyXL._

import java.util.concurrent.atomic.AtomicInteger
import starling.utils.ImplicitConversions._


trait Handler {
  def handle(request : Request) : Response;
}

abstract class TypedHandler[Input, Output](id: AtomicInteger, getInput : Request => Input,
                                           setOutput: (Response.Builder, Output) => Unit)
  extends Handler {

  def handle(request : Request) : Response = Response.newBuilder
    .setId(id.incrementAndGet)
    .setType(request.getType)
    .setRequestId(request.getId)
    .update { responseBuilder => handle(getInput(request)).update {
      case (output, status) => setOutput(responseBuilder, output); responseBuilder.setStatus(status);
    } }
    .build

  def handle(input : Input) : (Output, Response.Status)
}