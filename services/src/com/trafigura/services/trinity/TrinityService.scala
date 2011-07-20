package com.trafigura.services.trinity

import starling.services.rpc.{EDMFormats, JsonSerializer}
import javax.ws.rs._
import com.trafigura.services.{ServiceApi, ResteasyServiceApi}

case class TrinityService(service: ServiceApi) {
  val profile = service.create[ProfileService]
  val depoRates = service.create[DepoRatesService]
  val commodityRates = service.create[CommodityRatesService]
}

@Path("/Profile")
trait ProfileService {
  @Path("/{name}/{visibility}")
  @GET @Produces(Array("application/json"))
  def get(@PathParam("name") name: String, @PathParam("visibility") visibility: String): Profile
}

@Path("/DepoRate")
trait DepoRatesService {
  @Path("/{profileId}/{commodity}")
  @GET @Produces(Array("application/json"))
  def getRates(@PathParam("profileId") profileId: Int,
               @PathParam("commodity") commodity: String): List[DepoRate]

  @Path("/{profileId}/{commodity}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteRates(@PathParam("profileId") profileId: Int,
                  @PathParam("commodity") commodity: String): Boolean

  @Path("/{profileId}/{commodity}/{period}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteRate(@PathParam("profileId") profileId: Int,
                 @PathParam("commodity") commodity: String,
                 @PathParam("period") period: String): Boolean

  @Path("/{profileId}/{commodity}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setRates(@PathParam("profileId") profileId: Int,
               @PathParam("commodity") commodity: String,
               rates: List[DepoRate]): Boolean

  @Path("/{profileId}/{commodity}")
  @PUT @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def addRates(@PathParam("profileId") profileId: Int,
               @PathParam("commodity") commodity: String,
               rates: List[DepoRate]): Boolean
}

@Path("/CommodityRate")
trait CommodityRatesService {
  @Path("/{profileId}/{exchange}/{commodity}/{currency}")
  @GET @Produces(Array("application/json"))
  def getRates(@PathParam("profileId") profileId: Int,
               @PathParam("exchange") exchange: String,
               @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String): List[CommodityRate]

  @Path("/{profileId}/{exchange}/{commodity}/{currency}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteRates(@PathParam("profileId") profileId: Int,
                  @PathParam("exchange") exchange: String,
                  @PathParam("commodity") commodity: String,
                  @PathParam("currency") currency: String): Boolean

  @Path("/{profileId}/{exchange}/{commodity}/{currency}/{period}")
  @DELETE @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def deleteRate(@PathParam("profileId") profileId: Int,
                 @PathParam("exchange") exchange: String,
                 @PathParam("commodity") commodity: String,
                 @PathParam("currency") currency: String,
                 @PathParam("period") period: String): Boolean

  @Path("/{profileId}/{exchange}/{commodity}/{currency}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setRates(@PathParam("profileId") profileId: Int,
               @PathParam("exchange") exchange: String,
               @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String,
               rates: List[CommodityRate]): Boolean

  @Path("/{profileId}/{exchange}/{commodity}/{currency}")
  @PUT @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def addRates(@PathParam("profileId") profileId: Int,
               @PathParam("exchange") exchange: String,
               @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String,
               rates: List[CommodityRate]): Boolean
}


case class Profile(id: Int, name: String, visibility: String)
case class DepoRate(period: String, periodFromToday: Boolean, bid: Double, offer: Double, date: String)
case class CommodityRate(period: String, bid: Double, offer: Double, date: String, exchange: String,
                         contract: String, unitsTop: String, unitsBottom: String)

object TrinityClient {
  def main(args: Array[String]) {
    val services: ServiceApi = {
      val trinityTest   = ResteasyServiceApi("http://ttraflon2k196/trinity")
      val windows       = ResteasyServiceApi("http://localhost:9100/")
      val starlingLocal = ResteasyServiceApi("http://localhost:37220/RPC")

      trinityTest
    }

    val trinityService = TrinityService(services)

    val fullCurveProfile = trinityService.profile.get("Full Curve", "public")

    val depoRates = trinityService.depoRates.getRates(fullCurveProfile.id, "USD")

    depoRates.foreach(println)

    trinityService.depoRates.addRates(fullCurveProfile.id, "USD", List(
      DepoRate("2D",false,0.128,0.129,"26/07/2011"),
      DepoRate("1W",false,0.1607,0.1607,"01/08/2011"),
      DepoRate("2W",false,0.1695,0.1695,"08/08/2011")
    ));

    trinityService.depoRates.deleteRate(fullCurveProfile.id, "USD", "2D");

    val commodityRates = trinityService.commodityRates.getRates(fullCurveProfile.id, "SFE", "XPS", "RMB")

    commodityRates.foreach(println)
  }
}