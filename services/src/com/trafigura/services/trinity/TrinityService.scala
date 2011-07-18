package com.trafigura.services.trinity

import com.trafigura.services.ResteasyServiceApi
import starling.services.rpc.{EDMFormats, JsonSerializer}
import javax.ws.rs._

@Path("/Trinity")
trait TrinityService {
  @Path("Profile/{name}/{visibility}")
  @GET @Produces(Array("application/json"))
  def getProfile(@PathParam("name") name: String, @PathParam("visibility") visibility: String): Profile

  @Path("DepoRateCurve/{profileId}/{commodity}")
  @GET @Produces(Array("application/json"))
  def getDepoRateCurve(@PathParam("profileId") profileId: Int, @PathParam("commodity") commodity: String): List[DepoRate]

  @Path("DepoRateCurve/{profileId}/{commodity}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteDepoRateCurve(@PathParam("profileId") profileId: Int, @PathParam("commodity") commodity: String): Boolean

  @Path("DepoRateCurve/{profileId}/{commodity}/{period}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteDepoRate(@PathParam("profileId") profileId: Int,
                     @PathParam("commodity") commodity: String,
                     @PathParam("period") period: String): Boolean

  @Path("DepoRateCurve/{profileId}/{commodity}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setDepoRateCurve(@PathParam("profileId") profileId: Int,
                       @PathParam("commodity") commodity: String,
                       rates: List[DepoRate]): Boolean

  @Path("DepoRateCurve/{profileId}/{commodity}")
  @PUT @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def addDepoRateCurve(@PathParam("profileId") profileId: Int,
                       @PathParam("commodity") commodity: String,
                       rates: List[DepoRate]): Boolean
}

case class Profile(id: Int, name: String, visibility: String)
case class DepoRate(period: String, periodFromToday: Boolean, bid: Double, offer: Double, date: Int)

object TrinityClient {
  def main(args: Array[String]) {
    val services = {
      val trinityTest   = ResteasyServiceApi("http://ttraflon2k196/trinity")
      val windows       = ResteasyServiceApi("http://localhost:9100/")
      val starlingLocal = ResteasyServiceApi("http://localhost:37220/RPC")

      trinityTest
    }

    val trinityService = services.create[TrinityService]

    val fullCurveProfile = trinityService.getProfile("Full Curve", "public")

    val depoRates = trinityService.getDepoRateCurve(fullCurveProfile.id, "USD")

    depoRates.foreach(println)

    trinityService.addDepoRateCurve(fullCurveProfile.id, "USD", List(
      DepoRate("2D",false,0.128,0.129,40742),
      DepoRate("1W",false,0.1608,0.1608,40746),
      DepoRate("2W",false,0.1695,0.1695,40753)
    ));

    trinityService.deleteDepoRate(fullCurveProfile.id, "USD", "2D");
  }
}