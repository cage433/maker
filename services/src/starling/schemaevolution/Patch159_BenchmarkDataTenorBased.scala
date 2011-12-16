package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import system.Patch
import starling.marketdata._
import starling.db.{MarketDataEntry, MarketDataSet, DBWriter}
import starling.daterange.{ObservationPoint, Month, Tenor}
import starling.market._
import starling.quantity.Quantity
import starling.quantity.UOM._

class Patch159_BenchmarkDataTenorBased extends Patch {

  override def requiresRestart = true // because this patch modifies markets which may have been read already.

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val patchUtil = MarketDataPatchUtil(starling, writer)
    patchUtil.deleteMarketData(true, MarketDataTypeName("CountryBenchmark"))
    val m0 = Tenor(Month, 0)
    val m1 = Tenor(Month, 1)
    val entries = 
        List(
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("LEAD1")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("1L")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("3L")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("LEAD1")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("INGOT")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("3L")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(0.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("INGOT")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("3L")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("1L")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("LEAD1")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("1L")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("INGOT")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Lead), CountryBenchmarkData(Map((NeptuneCountryCode("AU1"), GradeCode("1L")) -> Map(m0 -> Quantity(25.025, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XHK"), GradeCode("SA00")) -> Map(m0 -> Quantity(23.456, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("NSA01")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAW"), GradeCode("NSA01")) -> Map(m1 -> Quantity(20.0, CNY/MT), m0 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("NSA00")) -> Map(m1 -> Quantity(250.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("SA00")) -> Map(m0 -> Quantity(300.0, CNY/MT), m1 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XTW"), GradeCode("NSA00")) -> Map(m0 -> Quantity(1000.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("AA")) -> Map(m0 -> Quantity(1234.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("NSA01")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("NSA01")) -> Map(m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAA"), GradeCode("SA00")) -> Map(m0 -> Quantity(300.0, CNY/MT), m1 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("NSA00")) -> Map(m1 -> Quantity(30.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAX"), GradeCode("SA00")) -> Map(m0 -> Quantity(200.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("NSA00")) -> Map(m0 -> Quantity(250.0, CNY/MT), m1 -> Quantity(80.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAX"), GradeCode("NSA01")) -> Map(m0 -> Quantity(100.0, CNY/MT), m1 -> Quantity(20.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAX"), GradeCode("NSA00")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAW"), GradeCode("NSA00")) -> Map(m1 -> Quantity(30.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("NSA00")) -> Map(m0 -> Quantity(9000.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("SA00")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("NSA01")) -> Map(m1 -> Quantity(20.0, CNY/MT), m0 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("SA00")) -> Map(m0 -> Quantity(300.0, CNY/MT), m1 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("HMC"), GradeCode("A0")) -> Map(m0 -> Quantity(12.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("AAW"), GradeCode("SA00")) -> Map(m0 -> Quantity(200.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Aluminium), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("NSA00")) -> Map(m1 -> Quantity(80.0, CNY/MT), m0 -> Quantity(250.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(AluminiumAlloy), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("AA")) -> Map(m0 -> Quantity(1000.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("SHGC")) -> Map(Tenor(Month, 4) -> Quantity(-50.0, CNY/MT), m0 -> Quantity(-100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("NSSC")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("STAND")) -> Map(m1 -> Quantity(100.0, CNY/MT), m0 -> Quantity(250.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XTW"), GradeCode("STAND")) -> Map(m0 -> Quantity(1000.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XHK"), GradeCode("#1SCP")) -> Map(m0 -> Quantity(100.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("OC")) -> Map(m1 -> Quantity(-600.0, CNY/MT), m0 -> Quantity(-600.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("CHB"), GradeCode("STAND")) -> Map(m0 -> Quantity(3525.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("STAND")) -> Map(m0 -> Quantity(-50.0, CNY/MT), m1 -> Quantity(0.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XHK"), GradeCode("#2SCP")) -> Map(m0 -> Quantity(34.12, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("OC")) -> Map(m0 -> Quantity(-600.0, CNY/MT), m1 -> Quantity(-600.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("SHGC")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(200.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("STAND")) -> Map(m1 -> Quantity(100.0, CNY/MT), m0 -> Quantity(250.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("SHSCU")) -> Map(m1 -> Quantity(-100.0, CNY/MT), m0 -> Quantity(-150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("OFFG")) -> Map(m1 -> Quantity(-600.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("#1SCP")) -> Map(m0 -> Quantity(150.45, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("STAND")) -> Map(m0 -> Quantity(1000.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("OC")) -> Map(m0 -> Quantity(-600.0, CNY/MT), m1 -> Quantity(-600.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("SHSCU")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Copper), CountryBenchmarkData(Map((NeptuneCountryCode("STO"), GradeCode("#2SCP")) -> Map(m0 -> Quantity(13.4, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Nickel), CountryBenchmarkData(Map((NeptuneCountryCode("XTW"), GradeCode("FPC")) -> Map(m0 -> Quantity(134.34, EUR/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Nickel), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("FPC")) -> Map(m0 -> Quantity(1282.0, CNY/MT), m1 -> Quantity(1282.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("S0Z")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("S0Z")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("NS0Z")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XQD"), GradeCode("NS0Z")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("NS0Z")) -> Map(m1 -> Quantity(50.0, CNY/MT), m0 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XQD"), GradeCode("S0Z")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("S0Z")) -> Map(m1 -> Quantity(-250.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XQD"), GradeCode("NS1ZN")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("CGG")) -> Map(m0 -> Quantity(50.0, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XTJ"), GradeCode("NS1ZN")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("CWU"), GradeCode("S0Z")) -> Map(m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("NS1ZN")) -> Map(m1 -> Quantity(-250.0, CNY/MT), m0 -> Quantity(150.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XHK"), GradeCode("NS0Z")) -> Map(m0 -> Quantity(11.2, USD/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("CTU"), GradeCode("NS1ZN")) -> Map(m0 -> Quantity(50.0, CNY/MT), m1 -> Quantity(50.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XSH"), GradeCode("NS0Z")) -> Map(Tenor(Month, 12) -> Quantity(11.32, CNY/MT), m0 -> Quantity(150.0, CNY/MT), m1 -> Quantity(-250.0, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Zinc), CountryBenchmarkData(Map((NeptuneCountryCode("XIT"), GradeCode("NS0Z")) -> Map(m0 -> Quantity(13.4, CNY/MT))))),
          MarketDataEntry(ObservationPoint.RealTime, CountryBenchmarkMarketDataKey(Steel), CountryBenchmarkData(Map((NeptuneCountryCode("XTW"), GradeCode("MESB")) -> Map(m0 -> Quantity(1000.0, USD/MT)))))
        )

    starlingInit.marketDataStore.save(Map(
      MarketDataSet.ManualMetals  → entries))
  }
}
