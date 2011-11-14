package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.{MarketDataEntry, MarketDataSet, DBWriter}
import starling.daterange.{ObservationTimeOfDay, ObservationPoint, Day}
import starling.quantity.{Quantity, UOM}
import starling.marketdata.{ShanghaiVATData, ShanghaiVATDataKey}

class Patch141_WriteShanghaiVATRate extends Patch{
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val entries = Day(2011, 10, 1).until(Day.today).map{
      day =>
        MarketDataEntry(ObservationPoint(day, ObservationTimeOfDay.Default), ShanghaiVATDataKey(), ShanghaiVATData(Quantity(17, UOM.PERCENT)))
    }.toList
    starlingInit.marketDataStore.save(Map(
      MarketDataSet.ManualMetals → entries))
  }

}