package starling.curves

import org.scalatest.matchers.ShouldMatchers
import starling.utils.StarlingTest
import org.testng.annotations.Test
import CurveViewerFields._
import starling.market.Market
import starling.quantity.{Quantity, UOM}
import starling.daterange._
import starling.utils.ImplicitConversions._
import starling.pivot.{Field, PivotFieldsState}
import collection.immutable.{Set, TreeMap}
import starling.marketdata._


class CurveViewerInputsPivotTableDataSourceTests  extends StarlingTest with ShouldMatchers {

  lazy val pivot = new CurveViewerInputsPivotTableDataSource(inputs.toSet)

  val spotFXData = new SpotFXData(Quantity(6.7, UOM.CNY / UOM.USD))
  val market = Market.LME_LEAD
  val priceData = PriceData.create(List(Month(2010, 1) → 50.0, Month(2010, 1) → 60.0), market.priceUOM)
  val day = Day.today

  val inputs = List(
    (ObservationPoint(day, ObservationTimeOfDay.Default), SpotFXDataKey(UOM.CNY), spotFXData),
    (ObservationPoint(day, ObservationTimeOfDay.LMEClose), PriceDataKey(market), priceData)
  )

  @Test
  def inputsShouldBeReturnedAsRows() {
    val rows = pivot.data(PivotFieldsState(dataFields = List(inputField), rowFields = List(periodField))).data

    val expectedPeriods = None :: (priceData.prices.keys.toList).map(Some(_))
    rows.mapDistinct(map => map.get(periodField)) should be === expectedPeriods.toSet

    val expectedMarkets = List("CNY", market.name)
    rows.mapDistinct(map => map(marketField)) should be === expectedMarkets.toSet

    val expectedTimesOfDay = List(ObservationTimeOfDay.LMEClose, ObservationTimeOfDay.Default)
    rows.mapDistinct(map => map(timeOfDayField)) should be === expectedTimesOfDay.toSet
  }
}