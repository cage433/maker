package com.trafigura.services.trinity

import javax.ws.rs._
import com.trafigura.services.{ServiceApi, ResteasyServiceApi}
import starling.utils.{Log, Stopable}


class TrinityService(service: => ServiceApi) extends Stopable with Log {
  lazy val profile = service.create[ProfileService]
  lazy val depoRates = service.create[DepoRatesService]
  lazy val commodityRates = service.create[CommodityRatesService]

  override def start = { // defer binding until after resteasy has been initialised
    Log.debug("Trinity services: " + (profile, depoRates, commodityRates))
  }
}

@Path("/Profile")
trait ProfileService {
  @Path("{name}/{visibility}")
  @GET @Produces(Array("application/json"))
  def get(@PathParam("name") name: String, @PathParam("visibility") visibility: String): Profile

  @Path("{name}/{visibility}/{date}")
  @GET @Produces(Array("application/json"))
  def getByDate(@PathParam("name") name: String, @PathParam("visibility") visibility: String, @PathParam("date") date: String): Profile
}

@Path("/DepoRate")
trait DepoRatesService {
  @Path("{commodity}/{profileName}")
  @GET @Produces(Array("application/json"))
  def getRates(@PathParam("commodity") commodity: String, @PathParam("profileName") profileName: String): List[DepoRate]

  @Path("{commodity}/{profileName}")
  @DELETE @Produces(Array("application/json"))
  def deleteRates(@PathParam("commodity") commodity: String, @PathParam("profileName") profileName: String): Boolean

  @Path("{commodity}/{period}/{profileName}")
  @DELETE @Produces(Array("application/json"))
  def deleteRate(@PathParam("commodity") commodity: String, @PathParam("period") period: String, @PathParam("profileName") profileName: String): Boolean

  @Path("{commodity}/{profileName}")
  @PUT @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def putRates(@PathParam("commodity") commodity: String, @PathParam("profileName") profileName: String, rates: List[DepoRate]): Boolean

  @Path("{commodity}/{profileName}")
  @POST @Consumes(Array("application/json"))@Produces(Array("application/json"))
  def addRates(@PathParam("commodity") commodity: String, @PathParam("profileName") profileName: String, rates: List[DepoRate]): Boolean
}

@Path("/CommodityRate")
trait CommodityRatesService {
  @Path("{exchange}/{commodity}/{currency}/{profileName}")
  @GET @Produces(Array("application/json"))
  def getRates(@PathParam("exchange") exchange: String, @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String, @PathParam("profileName") profileName: String): List[CommodityRate]

  @Path("{exchange}/{commodity}/{currency}/{profileName}")
  @DELETE @Produces(Array("application/json"))
  def deleteRates(@PathParam("exchange") exchange: String, @PathParam("commodity") commodity: String,
                  @PathParam("currency") currency: String, @PathParam("profileName") profileName: String): Boolean

  @Path("{exchange}/{commodity}/{currency}/{period}/{profileName}")
  @DELETE @Produces(Array("application/json"))
  def deleteRate(@PathParam("exchange") exchange: String, @PathParam("commodity") commodity: String,
                 @PathParam("currency") currency: String, @PathParam("period") period: String, @PathParam("profileName") profileName: String): Boolean

  @Path("{exchange}/{commodity}/{currency}/{profileName}")
  @PUT @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def putRates(@PathParam("exchange") exchange: String, @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String, @PathParam("profileName") profileName: String, rates: List[CommodityRate]): Boolean

  @Path("{exchange}/{commodity}/{currency}/{profileName}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def addRates(@PathParam("exchange") exchange: String, @PathParam("commodity") commodity: String,
               @PathParam("currency") currency: String, @PathParam("profileName") profileName: String, rates: List[CommodityRate]): Boolean
}

case class Profile(id: Int, name: String, visibility: String)
case class DepoRate(period: String, periodFromToday: Boolean, bid: Double, offer: Double, date: String)
case class CommodityRate(period: String, bid: Double, offer: Double, date: String, exchange: String,
                         contract: String, unitsTop: String, unitsBottom: String)

object TrinityClient {
  def main(args: Array[String]) {
    val services: ServiceApi = {
      val trinityTest   = ResteasyServiceApi("http://ttraflon2k196:9100")
      val windows       = ResteasyServiceApi("http://localhost:9100/")
      val starlingLocal = ResteasyServiceApi("http://localhost:37220/RPC")

      trinityTest
    }

    val trinityService = new TrinityService(services)

    val depoRates = trinityService.depoRates.getRates("USD", "Full Curve")

    depoRates.foreach(println)

    trinityService.depoRates.addRates("USD", "Full Curve", List(
      DepoRate("2D",false,0.128,0.129,"26/07/2011"),
      DepoRate("1W",false,0.1607,0.1607,"01/08/2011"),
      DepoRate("2W",false,0.1695,0.1695,"08/08/2011")
    ));

    trinityService.depoRates.deleteRate("USD", "2D", "Full Curve");

    val commodityRates = trinityService.commodityRates.getRates("SFE", "XPS", "RMB", "Full Curve")

    commodityRates.foreach(println)

    println("head commodity rate: " + commodityRates.head)

//    trinityService.commodityRates.putRates("SFE", "XPS", "RMB", "Full Curve", commodityRates.tail)
  }
}