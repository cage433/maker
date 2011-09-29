package starling.client

import org.joda.time.LocalDate
import com.trafigura.services._
import com.trafigura.services.marketdata._


object MarketDataClient {
  def main(args: Array[String]) = test(BouncyRMIServiceApi())

  def test(serviceApi: ServiceApi) = serviceApi.using { marketData: MarketDataServiceApi =>

    val date = new LocalDate(2011, 9, 14)
    val observationDay = new TitanSerializableDate(date)
    val snapshotId = new TitanSnapshotIdentifier("4439", date)

    ///////////////////
    // Spot FX Examples
    ///////////////////

    { // Latest SpotFX
      val latestSnapshot: TitanSnapshotIdentifier = marketData.latestSnapshotID().getOrElse(throw new Exception("No snapshots"))
      val latestSpotFXRates: List[SpotFXRate] = marketData.getSpotFXRates(latestSnapshot, observationDay)

      println("latest getSpotFXRates:...")
      latestSpotFXRates.foreach(println)
    }

    { // SpotFX for a given snapshot
      val spotFXRates: List[SpotFXRate] = marketData.getSpotFXRates(snapshotId, observationDay)

      println("getSpotFXRates:...")
      spotFXRates.foreach(println)
    }

    { // Specific SpotFX
      val currencies = TitanSerializableCurrency.titanCurrencies.map(TitanSerializableCurrency(_))

      for (from <- currencies; to <- currencies) {
        val spotFXRate: SpotFXRate = marketData.getSpotFXRate(snapshotId, observationDay, from, to)

        println("getSpotFXRate(%s, %s, %s) = %s".format(snapshotId, from, to, spotFXRate))
      }
    }

    ///////////////////////////////////
    // Reference Interest Rate Examples
    ///////////////////////////////////

    {
      val interestRates: List[ReferenceInterestRate] = marketData.getReferenceInterestRates(snapshotId, observationDay)

      println("getReferenceInterestRates:...")
      interestRates.foreach(println)
    }

    {
      val interestRate: ReferenceInterestRate = marketData.getReferenceInterestRate(
        snapshotId, observationDay, ReferenceRateSource("LIBOR"), Maturity.get("1M"), TitanSerializableCurrency("GBP"))

      println("getReferenceInterestRate: " + interestRate)
    }

    {
      val domesticRates: List[ReferenceInterestRate] = {
        val from = TitanSerializableDate(new LocalDate(2011, 7, 1))
        val to = TitanSerializableDate(new LocalDate(2011, 7, 6))

        marketData.getReferenceInterestRates(snapshotId, from, to, ReferenceRateSource("Domestic Rate"),
          Maturity.get("ON"), TitanSerializableCurrency("RMB"))
      }

      domesticRates.foreach(println)
    }
  }
}