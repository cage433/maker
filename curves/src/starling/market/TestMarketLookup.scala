package starling.market

import starling.utils.StringIO
import starling.calendar.{HolidayTablesFactory, BusinessCalendars}
import io.Source
import concurrent.stm._
import scalaz.Scalaz._

object TestMarketCreator extends MarketLookupCreator {
  def create = TestMarketLookup
}

case object TestMarketLookup extends MarketLookup {
  lazy val file = Source.fromURL(getClass.getResource("/starling/market/Markets.csv")).getLines.toList
  //  val file = StringIO.lines("/starling/market/Markets.csv").toList
  lazy val header = file.head.split('\t').map(_.toLowerCase).zipWithIndex.toMap
  lazy val lines = file.tail.map {
    line => {
      val entries = line.split('\t')
      new MarketParser.Line {
        def get(name:String) = {
          val index = header(name.toLowerCase)
          entries(index).trim
        }

        override def toString = line
      }
    }
  }

  lazy val businessCalendars = new BusinessCalendars(HolidayTablesFactory.holidayTables)
  lazy val expiryRules = FuturesExpiryRuleFactory.expiryRules

  lazy val marketParser = new MarketParser(businessCalendars, expiryRules)

  lazy val all = Ref(marketParser.fromLines(lines))

  def add(m:FuturesMarket) = atomic {
    implicit txn => all() = Left(m) :: all()
  }

  def remove(m:FuturesMarket) = atomic {
    implicit txn => all() = all().span(_.left.toOption != Some(m)) |> (s => s._1 ::: s._2.tail)
  }

  def allIndexes = all.single().flatMap {
    case Right(i:Index) => Some(i)
    case _ => None
  }

  def allMarkets = all.single().flatMap {
    case Left(m:FuturesMarket) => Some(m)
    case _ => None
  }
}
