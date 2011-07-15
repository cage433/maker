package com.trafigura.services.trinity

import com.trafigura.services.ResteasyServiceApi
import javax.ws.rs._
import starling.services.rpc.{EDMFormats, JsonSerializer}

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

  @Path("DepoRateCurve/{profileId}/{commodity}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setDepoRateCurve(@PathParam("profileId") profileId: Int,
                       @PathParam("commodity") commodity: String,
                       rates: List[DepoRate]): List[DepoRate]
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

    val depoRates = trinityService.getDepoRateCurve(fullCurveProfile.id, "' or '1'='1")

    depoRates.foreach(println)

    if (true) return

    {
      trinityService.deleteDepoRateCurve(fullCurveProfile.id, "USD")

      {
        val ratesAfterDeletion = trinityService.getDepoRateCurve(fullCurveProfile.id, "USD")
        println("after deletion...")
        ratesAfterDeletion.foreach(println)
      }

      {
      }
      val result = {
        val newRates = List(
          DepoRate("1D",false,0.128,0.128,40742),
          DepoRate("1W",false,0.1608,0.1608,40746),
          DepoRate("2W",false,0.1695,0.1695,40753),
          DepoRate("1M",false,0.18560000000000001,0.18560000000000001,40770),
          DepoRate("2M",false,0.21730000000000002,0.21730000000000002,40801),
          DepoRate("3M",false,0.24580000000000002,0.24580000000000002,40833),
          DepoRate("6M",false,0.39780000000000004,0.39780000000000004,40925),
          DepoRate("9M",false,0.5638000000000001,0.5638000000000001,41015),
          DepoRate("1Y",false,0.7335,0.7335,41106)
        )

//        val newRates = depoRates.map(rate => rate.copy(offer = rate.offer * 2))

        trinityService.setDepoRateCurve(fullCurveProfile.id, "USD", newRates)
      }

      result.foreach(println)
    }

  }
}