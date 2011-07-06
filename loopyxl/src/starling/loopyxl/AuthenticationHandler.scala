package starling.loopyxl

import LoopyXL._
import Response.Status._
import starling.bouncyrmi.AuthHandler
import java.util.concurrent.atomic.AtomicInteger
import starling.loopyxl.LoopyXL.Response.Status
import starling.auth.User


class AuthenticationHandler(id: AtomicInteger, authHandler: AuthHandler[User])
  extends TypedHandler[AuthenticateRequest, AuthenticateResponse](
  id, request => request.getAuthenticate, (builder, authenticate) => builder.setAuthenticate(authenticate)) {

  def handle(authenticate: AuthenticateRequest) =
    AuthenticateResponse.newBuilder.build → authorize(authenticate.getTicket.toByteArray)

  private def authorize(ticket: Array[Byte]) : Status = authHandler.authorized(Some(ticket)) match {
    case None => FAILURE
    case Some(_) => SUCCESS
  }
}
