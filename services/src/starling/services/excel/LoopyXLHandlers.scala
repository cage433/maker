package starling.services.excel

import starling.loopyxl._
import LoopyXL._


class LoopyXLHandlers(handlers0: (LoopyXL.MessageType, Handler)*) extends Handler {
  private val handlers = handlers0.toMap

  def handle(request: Request) = handlers(request.getType).handle(request)
}

