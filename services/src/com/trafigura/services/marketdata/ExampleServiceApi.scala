package com.trafigura.services.marketdata

import javax.ws.rs._
import starling.daterange.Day
import com.trafigura.services.{TitanSerializablePercentage, TitanSerializableCurrency, TitanSerializableDate}
import org.jboss.resteasy.client.ProxyFactory
import org.jboss.resteasy.spi.ResteasyProviderFactory
import starling.services.rpc.JsonDeserializerMessageBodyReader


@Path("/Example")
trait ExampleServiceApi {
  @Path("EchoReferenceInterestRate")
  @GET @Consumes(Array("application/json")) @Produces(Array("application/json"))
  def echoReferenceInterestRate(referenceInterestRate: ReferenceInterestRate): ReferenceInterestRate

  @Path("ReferenceInterestRate")
  @GET @Produces(Array("application/json"))
  def getReferenceInterestRate(): ReferenceInterestRate
}


class ExampleService extends ExampleServiceApi {
  def echoReferenceInterestRate(referenceInterestRate: ReferenceInterestRate) = referenceInterestRate
  def getReferenceInterestRate() = ReferenceInterestRate(TitanSerializableDate(Day.today.toLocalDate),
    ReferenceRateSource("LIBOR"), NamedMaturity.ON, TitanSerializableCurrency("GBP"), TitanSerializablePercentage(0.123))
}

object ExampleClient {
  def main(args: Array[String]) {
    ResteasyProviderFactory.getInstance.registerProviderInstance(new JsonDeserializerMessageBodyReader)

    val exampleService = ProxyFactory.create(classOf[ExampleServiceApi], "http://localhost:9100/")

    println(exampleService.getReferenceInterestRate)
  }
}

