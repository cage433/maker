package starling.services

import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import starling.market.FuturesExchange
import starling.daterange.Day
import starling.pivot.{PivotFieldsState, PivotTableDataSource}
import starling.pivot.controller.PivotGrid
import swing.event.Event
import starling.utils.StarlingSpec
import starling.gui.api.Email

class VerifyPricesValidTests extends StarlingSpec with ShouldMatchers {
  "should send no emails when no prices available" in {
    verifier.withNoPrices.emailFor(Day.today) should be === None
  }

  def verifier() = {
    val dataSource = mock(classOf[PivotTableDataSource])
    val futuresExchange = mock(classOf[FuturesExchange])
    val emailService = mock(classOf[EmailService])

    new VerifyPricesValid(dataSource, futuresExchange, emailService, Email("from@example.org", "to@example.org")) {
      val emptyGrid = PivotGrid(Array(), Array(), Array())

      def withNoPrices = updateThis {
        when(dataSource.gridFor(any(classOf[Option[PivotFieldsState]]))) thenReturn emptyGrid
      }

      private def updateThis(action: Any) = { action; this }
      private def anyEvent: Event = any(classOf[Event])
    }
  }
}


