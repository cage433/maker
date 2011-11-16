package starling.webservice

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.daterange.Day._
import com.trafigura.services.marketdata.{NamedMaturity, ReferenceRateSource, ReferenceInterestRate}
import com.trafigura.services.{TitanSerializablePercentage, TitanSerializableCurrency, TitanSerializableDate}
import RichJValue._
import xml._


class XmlSerializerMessageBodyWriterTests extends WordSpec with ShouldMatchers {
  val writer = new XmlSerializerMessageBodyWriter

  val referenceInterestRate = ReferenceInterestRate(TitanSerializableDate((12 Jun 2011).toLocalDate),
    ReferenceRateSource("LIBOR"), NamedMaturity.ON, TitanSerializableCurrency("GBP"), TitanSerializablePercentage(0.12345))
  implicit val formats = EDMFormats

  "should produce xml with same structure as json" in {
    writer.serialize(classOf[ReferenceInterestRate], referenceInterestRate) should be === Utility.trim(
      <result type={className[ReferenceInterestRate]}>
        <observation-date type={className[TitanSerializableDate]} value="2011-06-12"></observation-date>
        <source type={className[ReferenceRateSource]} name="LIBOR"></source>
        <maturity type={className[NamedMaturity]} name="ON"></maturity>
        <currency type={className[TitanSerializableCurrency]} name="GBP"></currency>
        <rate type={className[TitanSerializablePercentage]} value="0.12345"></rate>
      </result>)
  }

  "should produce xml that can be converted to json" in {
    val serializer = JsonSerializer(classOf[ReferenceInterestRate])

    serializer.toJValue(referenceInterestRate).toXml should be ===
      writer.serialize(classOf[ReferenceInterestRate], referenceInterestRate)

    serializer.toJValue(referenceInterestRate).toXml.toJSON.pretty should be ===
      serializer.toJValue(referenceInterestRate).pretty

    writer.serialize(classOf[ReferenceInterestRate], referenceInterestRate).toJSON.pretty should be ===
      serializer.toJValue(referenceInterestRate).pretty
  }

  private def className[T: Manifest] = implicitly[Manifest[T]].erasure.getName
}