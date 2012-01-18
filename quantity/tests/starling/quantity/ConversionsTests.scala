package starling.quantity

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import starling.quantity.UOM._

class ConversionsTests extends TestNGSuite with ShouldMatchers {

  val bblPerMT = 50
  val map = Map(UOM.BBL / UOM.MT -> BigDecimal(bblPerMT))

  val conversions = Conversions(map)

  @Test
  def shouldConvertWellDefined {
    val a = USD/BBL
    val b = US_CENT/BBL
    val c = conversions.convert(a, b)
    c should be (Some(BigDecimal(100)))
  }

  @Test
  def shouldConvert {
    val a = USD/BBL
    val b = USD/MT
    val c = conversions.convert(a, b)
    c should be (Some(BigDecimal(50)))
  }

  @Test
  def shouldContainInverses {
    val inverseConversions: Iterable[(UOM, Option[BigDecimal], Option[BigDecimal])] =
      map.map {
        case (uom, conversion) =>
          (uom, Some((BigDecimal(1) / conversion)), conversions.convert(uom.numeratorUOM, uom.denominatorUOM))
      }

    inverseConversions.foreach {
      case (uom, expectedInverse, actualInverse) => {
        actualInverse should be === expectedInverse
      }
    }
  }
}
