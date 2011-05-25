package starling.schemaevolution.xstream

import starling.utils.StarlingTest
import org.testng.annotations.Test
import com.thoughtworks.xstream.XStream
import starling.utils.StarlingXStream
import org.testng.Assert
import starling.quantity.Percentage
import starling.marketdata.{ImpliedVolEntryKey, ImpliedVolData}
import starling.daterange.Day
import scala.collection.SortedMap
import java.lang.Class

case class Foo(x : String, y : Int)
case class Bar(x : String)

case class Dave(x : String)

class MapBasedXStreamConverterTests extends StarlingTest {

  @Test
  def testWeCanAddAField{
    /*
      The following xml was created an instance of 'case class Foo(x : String)'.
      We have then changed the class to be 'case class Foo(x : String, y : Int)' and
      want to convert the original xml, using a default value of 'y' of 38
     */
    val originalFooXml = """
    <starling.schemaevolution.xstream.Foo>
      <x>fred</x>
    </starling.schemaevolution.xstream.Foo>
    """

    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(
      new MapBasedConverter(
        StarlingXStream.createXStream,
        classOf[Foo],
        new Reader() {
          def create(fields: Fields) = {
            val x = fields.getFieldValue("x").asInstanceOf[Option[String]].get
            Foo(x, 37)
          }
        },
        Map.empty[String, Class[_]]
    ))
    Assert.assertEquals(
      convertingXStream.fromXML(originalFooXml),
      Foo("fred", 37)
    )

  }
  @Test
  def testTheConverterRecurses{

    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(
      new MapBasedConverter(
        StarlingXStream.createXStream,
        classOf[Foo],
        new Reader() {
          def create(fields: Fields) = {
            val x = fields.getFieldValue("x").asInstanceOf[Option[String]].get
            val y = fields.getFieldValue("y").asInstanceOf[Option[Int]].get
            Foo(x, y * 2)
          }
        },
        Map.empty[String, Class[_]]
    ))

    Assert.assertEquals(
      convertingXStream.fromXML(StarlingXStream.createXStream.toXML(List(Foo("mike", 20), Bar("bill")))),
      List(Foo("mike", 40), Bar("bill"))
    )
  }

  @Test
  def testThatWeCanDropAField{
    // Test we can convert a Foo to a Foo2
    val originalXStream = StarlingXStream.createXStream

    /*
      The following xml was created an instance of 'case class Dave(x : String, y : String)'.
      We have then changed the class to be 'case class Dave(x : String)' and
      want to convert the original xml, appending the value of y to that of x
     */
    val originalDaveXml = """
    <starling.schemaevolution.xstream.Dave>
      <y>mike</y>
      <x>fred</x>
    </starling.schemaevolution.xstream.Dave>
    """

    val fieldDroppingConverter = new MapBasedConverter(
      StarlingXStream.createXStream,
      classOf[Dave],
      new Reader() {
        def create(fields: Fields) = {
          val x = fields.getFieldValue("x").asInstanceOf[Option[String]].get
          val y = fields.getFieldValue("y").asInstanceOf[Option[String]].get
          Dave(x + y)
        }
      },
      Map[String, Class[_]]("y" -> classOf[String])
      )
    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(fieldDroppingConverter)
    Assert.assertEquals(
      convertingXStream.fromXML(originalDaveXml),
      Dave("fredmike")
    )
  }

  @Test
  def testConvertClassWhichCanBeRead {
    val original = StarlingXStream.write(Bar("OriginalValue"))
    val convertingXStream = StarlingXStream.createXStream
    convertingXStream.registerConverter(new ModifyClassWhichCanBeReadConverter() {
      def classToFix = classOf[Bar]
      def fix(any: AnyRef) = {
        any.asInstanceOf[Bar].copy(x="Fixed")
      }
    })
    Assert.assertEquals(
      convertingXStream.fromXML(original),
      Bar("Fixed")
    )
  }

//  @Test
//  def testWeCanDropAnotherField{
//    val xml = """
//      <starling.marketdata.ImpliedVolData>
//          <surface class="scala.collection.jcl.SortedMap$$anon$2">
//               <no-comparator/>
//                   <entry>
//                          <starling.marketdata.ImpliedVolEntryKey>
//                                  <exerciseDay>21May2008</exerciseDay>
//                                  <strike>950.0</strike>
//                                  <lastTrading>23May2008</lastTrading>
//                          </starling.marketdata.ImpliedVolEntryKey>
//                          <starling.quantity.Percentage>
//                            <value>0.2674194</value>
//                          </starling.quantity.Percentage>
//                   </entry>
//          </surface>
//          <uom>oz</uom>
//      </starling.marketdata.ImpliedVolData>"""
//
//    val converter = new MapBasedConverter(
//      StarlingXStream.createXStream,
//      classOf[ImpliedVolData],
//      new Reader(){
//        def create(fields: Fields) = {
//          val surface = fields.getFieldValue("surface").asInstanceOf[Option[SortedMap[ImpliedVolEntryKey, Percentage]]].get
//          ImpliedVolData(surface)
//        }
//      },
//      Map.empty[String, Class[_]]
//    )
//    val converter2 = new MapBasedConverter(
//      StarlingXStream.createXStream,
//      classOf[ImpliedVolEntryKey],
//      new Reader(){
//        def create(fields: Fields) = {
//          val exerciseDay =fields.getFieldValue("exerciseDay").asInstanceOf[Option[Day]].get
//          val lastTrading =fields.getFieldValue("lastTrading").asInstanceOf[Option[Day]].get
//          val strike      =fields.getFieldValue("strike").asInstanceOf[Option[Double]].get
//          ImpliedVolEntryKey(lastTrading, strike, exerciseDay)
//        }
//      },
//      Map[String, Class[_]]("lastTrading" -> classOf[Day])
//    )
//
//    val convertingXStream = StarlingXStream.createXStream
//    convertingXStream.registerConverter(converter)
//    convertingXStream.registerConverter(converter2)
//    val data = convertingXStream.fromXML(xml)
//
//     Assert.assertTrue(data.isInstanceOf[ImpliedVolData])
//
//  }
}

