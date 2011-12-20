package starling.schemaevolution

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import starling.instrument.utils.StarlingXStream
import starling.utils.ImplicitConversions._


class ConvertingPatcherTest extends WordSpec with ShouldMatchers {
  val patcher = new ConvertingPatcher[From, To]("from", Some("to"))

  "can convert top level class" in {
    patch(From(123)) should be === To(-123)
  }

  "can convert within another class" in {
    patch(AnotherClass(From(123))) should be === AnotherClass(To(-123))
  }

  "can convert within Some" in {
    patch(Some(From(123))) should be === Some(To(-123))
  }

  "can convert within List" in {
    patch(List(From(123), From(321))) should be === List(To(-123), To(-321))
  }

  "can convert within Either" in {
    patch(Left(From(123))) should be === Left(To(-123))
    patch(Right(From(123))) should be === Right(To(-123))
  }

  private def patch(value: Any): Any = toStream.fromXML(patcher.patch(fromStream.toXML(value)))
  private val fromStream = StarlingXStream.createXStream.update(_.alias("from", manifest[From].erasure))
  private val toStream = StarlingXStream.createXStream.update(_.alias("to", manifest[To].erasure))
}

case class From(value: Int) extends Convertable[To] {
  def convert = To(- value)
}

case class To(negatedValue: Int)

case class AnotherClass(value: Any)