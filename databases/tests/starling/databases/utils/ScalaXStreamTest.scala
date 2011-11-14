package starling.databases.utils

import org.testng.annotations.Test
import org.testng.Assert._

import starling.instrument.utils.StarlingXStream
import starling.utils.StarlingTest

case class Dummy(items: List[DummyItem])
case class DummyItem(name: String)

class ScalaXStreamTest extends StarlingTest {
  @Test
  def testList {
    val xml = <starling.databases.utils.Dummy>
               <items>
                <starling.databases.utils.DummyItem><name>Blah</name></starling.databases.utils.DummyItem>
                <starling.databases.utils.DummyItem><name>Yada</name></starling.databases.utils.DummyItem>
                <starling.databases.utils.DummyItem><name>Whatever!</name></starling.databases.utils.DummyItem>
               </items>
              </starling.databases.utils.Dummy>

    val xStream = StarlingXStream.createXStream
    val obj = xStream.fromXML(xml.toString)
    assertEquals(obj.getClass, classOf[Dummy])
    val dummy = obj.asInstanceOf[Dummy]
    assertEquals(dummy.items.size, 3)
    assertEquals(dummy.items.head.getClass, classOf[DummyItem])
  }
}
