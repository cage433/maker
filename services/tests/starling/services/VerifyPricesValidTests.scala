package starling.services

import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import starling.market.FuturesExchange
import starling.daterange.Day
import starling.pivot.{PivotFieldsState, PivotTableDataSource}
import starling.pivot.controller.PivotGrid
import swing.event.Event
import starling.utils.{Broadcaster, StarlingSpec}
import starling.gui.api.EmailEvent


class VerifyPricesValidTests extends StarlingSpec with ShouldMatchers {
  "should send no emails when no prices available" in {
    verifier.withNoPrices.eventFor(Day.today, new EmailEvent()) should be === None
  }

  def verifier() = {
    val dataSource = mock(classOf[PivotTableDataSource])
    val futuresExchange = mock(classOf[FuturesExchange])
    val broadcaster = mock(classOf[Broadcaster])

    new VerifyPricesValid(dataSource, futuresExchange, broadcaster, "from@example.org", "to@example.org") {
      val emptyGrid = PivotGrid(Array(), Array(), Array())

      def withNoPrices = updateThis {
        when(dataSource.gridFor(any(classOf[Option[PivotFieldsState]]))) thenReturn emptyGrid
      }

      private def updateThis(action: Any) = { action; this }
      private def anyEvent: Event = any(classOf[Event])
    }
  }
}


