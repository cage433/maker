package starling.webservice

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.utils.ImplicitConversions._
import java.util.ArrayList


class OptionTypeConverterTests extends WordSpec with ShouldMatchers {
  val converter = OptionTypeConverter(classOf[Option[_]], null)

  "Some(None) == None" in {
    converter.convert(arrayList(1)) should be === Some(1)
    converter.convert(arrayList()) should be === None
    converter.convert(arrayList(None)) should be === None
    converter.convert(arrayList(Some(1))) should be === Some(1)
  }

  private def arrayList(elements: Any*) = new ArrayList[Any]().update(list => elements.foreach(list.add(_)))
}