package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import xml.Utility
import starling.utils.XmlUtil._

class XmlUtilTests extends WordSpec with ShouldMatchers {
  val xml = Utility.trim(<result type="thing">
    <observation-date type="widget" value="12Jun2011"></observation-date>
    <source type="gadget" name="LIBOR"></source>
    <maturity type="whatsimacallit" name="ON"></maturity>
    <currency type="thingymabob" name="GBP"></currency>
    <rate type="oojemablonk" value="0.12345"></rate>
  </result>)

  "attributeToElement is inverse of elementToAttribute" in {
    elementToAttribute(attributeToElement(xml)) should be === xml
    attributeToElement(elementToAttribute(attributeToElement(xml))) should be === attributeToElement(xml)
  }
}