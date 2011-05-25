package starling.services

import org.scalatest.matchers.ShouldMatchers
import org.mockito.Mockito._
import org.mockito.Matchers._
import starling.market.FuturesExchange
import starling.daterange.Day
import starling.pivot.{PivotFieldsState, PivotTableDataSource}
import starling.pivot.controller.PivotGrid
import swing.event.Event
import starling.utils.{DelayedVerifier, Broadcaster, StarlingSpec}


class VerifyPricesValidTests extends StarlingSpec with ShouldMatchers {
  "should send no emails when no prices available" in {
    verifier.withNoPrices.shouldBroadcastNoEvents.execute(Day.today)
  }

  def verifier() = {
    val dataSource = mock(classOf[PivotTableDataSource])
    val futuresExchange = mock(classOf[FuturesExchange])
    val broadcaster = mock(classOf[Broadcaster])

    new VerifyPricesValid(dataSource, futuresExchange, broadcaster, props => props.MetalsEmailAddress) {
      val verify = DelayedVerifier()
      val emptyGrid = PivotGrid(Array(), Array(), Array())

      override def execute(observationDay: Day) = { super.execute(observationDay); verify() }

      def withNoPrices = updateThis {
        when(dataSource.gridFor(any(classOf[Option[PivotFieldsState]]))) thenReturn emptyGrid
      }

      def shouldBroadcastNoEvents = updateThis {
        verify.later(broadcaster, never)(_.broadcast(anyEvent))
      }

      private def updateThis(action: Any) = { action; this }
      private def anyEvent: Event = any(classOf[Event])
    }
  }
}


