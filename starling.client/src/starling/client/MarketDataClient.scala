package starling.client

import org.joda.time.LocalDate
import com.trafigura.services.valuation.TitanSnapshotIdentifier
import com.trafigura.services.marketdata.{ReferenceRateSource, MarketDataServiceApi, Maturity, ReferenceInterestRate}
import com.trafigura.services.{TitanSerializableCurrency, TitanSerializableDate, ServiceApi}

object MarketDataClient {
  def main(args: Array[String]) = test(BouncyRMIServiceApi(portOption = Some("37221")))

  def test(serviceApi: ServiceApi) = serviceApi.using { marketData: MarketDataServiceApi =>
    val date = new LocalDate(2011, 6, 24)
    val observationDay = Some(new TitanSerializableDate(date))
    val snapshotId = Some(new TitanSnapshotIdentifier("", date))

    val interestRates: List[ReferenceInterestRate] = marketData.getReferenceInterestRates(snapshotId, observationDay)

    println("getReferenceInterestRates:...")
    interestRates.foreach(println)

    val interestRate = marketData.getReferenceInterestRate(
      snapshotId, observationDay, ReferenceRateSource("LIBOR"), Maturity.get("1M"), TitanSerializableCurrency("GBP"))

    println("getReferenceInterestRate: " + interestRate)

    val domesticRates = {
      val from = TitanSerializableDate(new LocalDate(2011, 7, 1))
      val to = TitanSerializableDate(new LocalDate(2011, 7, 6))

      marketData.getReferenceInterestRates(snapshotId, from, to, ReferenceRateSource("Domestic Rate"),
        Maturity.get("ON"), TitanSerializableCurrency("RMB"))
    }

    domesticRates.foreach(println)

    val spotFXRates = marketData.getSpotFXRates(snapshotId, observationDay)

    println("getSpotFXRates:...")
    spotFXRates.foreach(println)

    val currencies = List("USD", "JPY", "GBP", "RMB").map(TitanSerializableCurrency(_))
    for (from <- currencies; to <- currencies) {
      val spotFXRate = marketData.getSpotFXRate(snapshotId, observationDay, from, to)

      println("getSpotFXRate(%s, %s, %s) = %s".format(snapshotId, from, to, spotFXRate))
    }
  }
}