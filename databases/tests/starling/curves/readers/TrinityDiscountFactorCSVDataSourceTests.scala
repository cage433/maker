package starling.curves.readers

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import starling.quantity.UOM
import starling.daterange.Day._
import starling.db.MarketDataEntry
import starling.daterange.{ObservationTimeOfDay, ObservationPoint}
import starling.marketdata.{ForwardRateDataEntry, ForwardRateData, ForwardRateDataKey}
import starling.utils.ImplicitConversions._
import ObservationTimeOfDay._


class TrinityDiscountFactorCSVDataSourceTests extends TestNGSuite with ShouldMatchers {
  val dataSource = new TrinityDiscountFactorCSVDataSource
  val observationDay = 05 Apr 2011

  import dataSource._

  @Test def shouldThrowOnMalformedLines {
    lineShouldThrow("")
    lineShouldThrow(",USD,06-Apr-2011,0.2517531819,0.9999586176")
    lineShouldThrow("ShortDepoSwap,,06-Apr-2011,0.2517531819,0.9999586176")
    lineShouldThrow("ShortDepoSwap,USD,,0.2517531819,0.9999586176")
    lineShouldThrow("ShortDepoSwap,USD,06-Apr-2011,,0.9999586176")
    lineShouldThrow("ShortDepoSwap,USD,06-Apr-2011,0.2517531819,")
    lineShouldThrow("ShortDepoSwap,USD,06-Apr-2011,0.2517531819")
    lineShouldThrow("ShortDepoSwap,USD,06-Apr-2011")
    lineShouldThrow("ShortDepoSwap,USD")
    lineShouldThrow("ShortDepoSwap")
    lineShouldThrow("ShortDepoSwap,badCurrency,06-Apr-2011,0.2517531819,0.9999586176")
    lineShouldThrow("ShortDepoSwap,USD,badDate,0.2517531819,0.9999586176")
    lineShouldThrow("ShortDepoSwap,USD,06-Apr-2011,0.2517531819,badNumber")
  }

  @Test def shouldNotCareIfUnusedRateIsMalformed {
    dataSource.parseLine(("ShortDepoSwap,USD,06-Apr-2011,badNumber,0.9999586176", 1))
  }

  @Test def shouldSkipNonDepoRows {
    dataSource.parseLine(("NONShortDepoSwap,USD,06-Apr-2011,1.234,0.9999586176", 1)) should be === None
  }

  @Test def shouldSkipParticularCurrencies {
    dataSource.parseLine(("ShortDepoSwap,%s,06-Apr-2011,1.234,0.9999586176" % ignoredCurrencies.head, 1)) should be === None
  }

  @Test def shouldThrowOnUnexpectedCurrency {
    intercept[Exception] {
      dataSource.parseLine(("ShortDepoSwap,ODDCCY,06-Apr-2011,1.234,0.9999586176", 1))
    }
  }

  @Test def shouldParseDepoRows {
    dataSource.parseLine(("ShortDepoSwap,USD,06-Apr-2011,1.234,0.9999586176", 1)) should be ===
      Some(Row.fromStrings("USD", "06-Apr-2011", "0.9999586176"))

    Row.fromStrings("USD", "06-Apr-2011", "0.9999586176") should be ===
      Row(UOM.USD, 06 Apr 2011, 0.9999586176)
  }

  @Test def shouldCreateMarketDataEntries {
    val expectedEntry = new MarketDataEntry(ObservationPoint(observationDay, Default), ForwardRateDataKey(UOM.USD),
      ForwardRateData(List(ForwardRateDataEntry(05 Apr 2011, "Discount", "ShortDepoSwap", 1.0),
                           ForwardRateDataEntry(06 Apr 2011, "Discount", "ShortDepoSwap", 0.8888586176))))

    dataSource.readLines(observationDay,
      List("ShortDepoSwap,USD,05-Apr-2011,1.234,1.0", "ShortDepoSwap,USD,06-Apr-2011,1.234,0.8888586176")) should
        be === List(expectedEntry)
  }

  @Test def shouldThrowWhenNoDataPresent {
    intercept[Exception] { dataSource.readLines(observationDay, Nil) }
  }

  def lineShouldThrow(line: String) = intercept[Exception](dataSource.parseLine((line, 1)))
}