package com.trafigura.services

import javax.ws.rs._

import javax.ws.rs.core._
import java.net.URI
import meta.WebService


trait DocumentedService {
  @Path("") @GET @Produces(Array("text/html"))
  def redirectToDocumentationService = Response.temporaryRedirect(new URI("/Doc/Forms/" + WebService.fromClass(getClass).uri)).build
}