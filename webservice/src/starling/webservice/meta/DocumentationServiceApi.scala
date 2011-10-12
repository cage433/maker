package starling.webservice.meta

import javax.ws.rs._
import javax.ws.rs.core._


@Path("/Doc")
trait DocumentationServiceApi {
  @Path("Files/{filename}") @GET
  def file(@PathParam("filename") filename: String): Response

  @Path("Forms/{serviceUri}") @GET @Produces(Array("text/html"))
  def forms(@PathParam("serviceUri") serviceUri: String): String

  @Path("Meta/{serviceUri}") @GET @Produces(Array("application/json"))
  def metaData(@PathParam("serviceUri") serviceUri: String): WebService

  @Path("Services") @GET @Produces(Array("application/json"))
  def allMetaData: List[WebService]
}