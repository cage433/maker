package starling.databases.utils

import org.testng.annotations.Test
import starling.marketdata.{ForwardRateDataEntry, ForwardRateData}
import org.testng.Assert._

import starling.instrument.utils.StarlingXStream
import starling.utils.StarlingTest

class ScalaXStreamTest extends StarlingTest {
  @Test
  def testList {
    val xml = """
<starling.marketdata.ForwardRateData>
  <entries>
      <starling.marketdata.ForwardRateDataEntry>
        <rate>0.00221711</rate>
        <trinityInstrumentType>DEPO</trinityInstrumentType>
        <forwardDay>22Apr2010</forwardDay>
      </starling.marketdata.ForwardRateDataEntry>
      <starling.marketdata.ForwardRateDataEntry>
        <rate>0.002864</rate>
        <trinityInstrumentType>DEPO</trinityInstrumentType>
        <forwardDay>19May2010</forwardDay>
      </starling.marketdata.ForwardRateDataEntry>
      <starling.marketdata.ForwardRateDataEntry>
        <rate>0.9825499999999999</rate>
        <trinityInstrumentType>FUTURE</trinityInstrumentType>
        <format>Yield</format>
        <forwardDay>16Jun2010</forwardDay>
      </starling.marketdata.ForwardRateDataEntry>
  </entries>
</starling.marketdata.ForwardRateData>
            """

    val xStream = StarlingXStream.createXStream
    val obj = xStream.fromXML(xml)
    assertEquals(obj.getClass, classOf[ForwardRateData])
    val frd = obj.asInstanceOf[ForwardRateData]
    assertEquals(frd.entries.size, 3)
    assertEquals(frd.entries.head.getClass, classOf[ForwardRateDataEntry])
  }
}
