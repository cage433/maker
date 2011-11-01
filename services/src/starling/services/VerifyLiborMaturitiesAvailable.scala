package starling.services

import starling.db.MarketDataStore
import starling.utils.Broadcaster

import starling.services.trinity.XRTGenerator._
import starling.utils.ImplicitConversions._
import starling.daterange.{Tenor, Day}
import starling.quantity.{Percentage, UOM}
import starling.scheduler.EmailingScheduledTask
import scalaz.Scalaz._
import starling.gui.api.Email


class VerifyLiborMaturitiesAvailable(marketDataStore: MarketDataStore, emailService: EmailService, template: Email)
  extends EmailingScheduledTask(emailService, template) {

  import starling.curves.readers.lim.LIBORFixing._

  protected def emailFor(observationDay: Day) = {
    val liborFixings: NestedMap[UOM, Tenor, (Percentage, Day)] = latestLiborFixings(marketDataStore, observationDay)
    val tenorsByCurrency = liborFixings.mapValues(_.keys.toList).withDefaultValue(Nil)
    val missingTenorsByCurrency = currencies.toMapWithValues(currency => tenorsFor(currency) \\ tenorsByCurrency(currency))
      .filterValuesNot(_.isEmpty).sortBy(_.toString)

    (missingTenorsByCurrency.size > 0).option {
      template.copy(subject = "Missing Libor Maturities in LIM, observation day: " + observationDay,
        body = <span>
                 <p>The following LIBOR tenors are required by Trinity but are missing in LIM</p>
                 <table>
                   { for ((currency, missingTenors) <- missingTenorsByCurrency) yield
                     <tr><td><b>{currency}: </b></td><td>{missingTenors.mkString(", ")}</td></tr>
                   }
                 </table>
               </span>.toString)
    }
  }
}