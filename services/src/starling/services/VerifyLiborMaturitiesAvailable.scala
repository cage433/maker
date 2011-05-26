package starling.services

import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api.EmailEvent
import starling.utils.Broadcaster

import starling.curves.readers.LIBORFixing._
import starling.services.trinity.XRTGenerator._
import starling.utils.ImplicitConversions._


class VerifyLiborMaturitiesAvailable(marketDataStore: MarketDataStore, broadcaster: Broadcaster, from: String, to: String*)
  extends EmailingScheduledTask(broadcaster, from, to) {

  protected def eventFor(observationDay: Day, email: EmailEvent) = {
    val tenorsByCurrency = latestLiborFixings(marketDataStore, observationDay).mapValues(_.keys.toList).withDefaultValue(Nil)
    val missingTenorsByCurrency = currencies.toMapWithValues(currency => tenors \\ tenorsByCurrency(currency)).sortBy(_.toString)

    (missingTenorsByCurrency.size > 0).toOption {
      email.copy(subject = "Missing Libor Maturities in LIM, observation day: " + observationDay,
        body = <html>
                 <p>The following LIBOR tenors are required by Trinity but are missing in LIM</p>
                 <table>
                   { for ((currency, missingTenors) <- missingTenorsByCurrency) yield
                     <tr><td><b>{currency}: </b></td><td>{missingTenors.mkString(", ")}</td></tr>
                   }
                 </table>
               </html>.toString)
    }
  }
}