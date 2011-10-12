package com.trafigura.services.marketdata

import javax.ws.rs._
import starling.daterange.Day
import collection.mutable.HashMap
import com.trafigura.services._



@Path("/Example")
trait ExampleServiceApi {
  @Path("ReferenceInterestRate/{source}") @Example("LIBOR")
  @GET @Produces(Array("application/json", "application/xml"))
  def getReferenceInterestRate(@PathParam("source") source: ReferenceRateSource): ReferenceInterestRate

  @Path("ReferenceInterestRates/{source}")
  @GET @Produces(Array("application/json")) @Example("JIBAR")
  def getReferenceInterestRates(@PathParam("source") source: ReferenceRateSource): List[ReferenceInterestRate]

  @Path("ReferenceInterestRate/{source}")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setReferenceInterestRate(@PathParam("source") source: ReferenceRateSource, rate: ReferenceInterestRate): ReferenceInterestRate

  @Path("ReferenceInterestRates")
  @POST @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def setReferenceInterestRates(rates: List[ReferenceInterestRate]): List[ReferenceInterestRate]

  @Path("ReferenceInterestRate/{source}")
  @DELETE @Produces(Array("application/json"))
  def deleteReferenceInterestRate(@PathParam("source") source: ReferenceRateSource): Boolean
}

object ExampleService extends ExampleServiceApi {
  private val rates: HashMap[ReferenceRateSource, ReferenceInterestRate] = new HashMap

  def getReferenceInterestRate(source: ReferenceRateSource) = {
    rates.getOrElseUpdate(source,
      ReferenceInterestRate(
        TitanSerializableDate(Day.today.toLocalDate), source, RelativeMaturity.get("1D"),
        TitanSerializableCurrency("GBP"), TitanSerializablePercentage(0.123)))
  }

  def getReferenceInterestRates(source: ReferenceRateSource) =
    List(getReferenceInterestRate(source), getReferenceInterestRate(source))

  def setReferenceInterestRate(source: ReferenceRateSource, rate: ReferenceInterestRate) = {
    rates(source) = rate

    rate
  }

  def setReferenceInterestRates(rates: List[ReferenceInterestRate]) = {
    rates.foreach(rate => this.rates(rate.source) = rate)

    rates
  }

  def deleteReferenceInterestRate(source: ReferenceRateSource) = rates.remove(source).isDefined
}

object ExampleClient {
  def main(args: Array[String]) {
    val services = {
      val trinityTest   = ResteasyServiceApi("http://ttraflon2k196:9100")
      val windows       = ResteasyServiceApi("http://localhost:9100/")
      val starlingLocal = ResteasyServiceApi("http://localhost:37214/RPC")

      starlingLocal
    }

    val exampleService = services.create[ExampleServiceApi]

    println(exampleService.getReferenceInterestRate(ReferenceRateSource("LIBOR")))

    val rate = ReferenceInterestRate(
      TitanSerializableDate(Day.today.toLocalDate), ReferenceRateSource("LIBOR"), NamedMaturity.ON,
      TitanSerializableCurrency("GBP"), TitanSerializablePercentage(0.987))

    exampleService.setReferenceInterestRate(ReferenceRateSource("LIBOR"),
      rate)

    println(exampleService.getReferenceInterestRate(ReferenceRateSource("BLAH")))
    println(exampleService.getReferenceInterestRate(ReferenceRateSource("LIBOR")))

    val result = exampleService.setReferenceInterestRates(List(rate, rate.copy(source = ReferenceRateSource("JIBAR"))))

    result.foreach(println)

    {
      val deleteResult = exampleService.deleteReferenceInterestRate(ReferenceRateSource("LIBOR"))
      println(deleteResult)
    }

    {
      val deleteResult = exampleService.deleteReferenceInterestRate(ReferenceRateSource("WIBBLE"))
      println(deleteResult)
    }
  }
}