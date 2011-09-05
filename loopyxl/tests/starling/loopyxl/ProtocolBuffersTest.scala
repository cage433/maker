package starling.loopyxl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import org.testng.Assert._

class ProtocolBuffersTest extends TestNGSuite with ShouldMatchers {
  import ProtocolBuffers._

  val arrayOfObjects = Array[Object](1.2.asInstanceOf[Object], "foo", 3.4.asInstanceOf[Object], "oof!")
  val matrixOfObjects = Array(arrayOfObjects, arrayOfObjects)

  @Test
  def shouldBeAbleToMarshallObjectArrays {
    val marshalled = objectArray(arrayOfObjects)
    val unmarshalled = fromValue(marshalled, arrayOfObjects.getClass)

    assertNotNull(unmarshalled)

    unmarshalled.asInstanceOf[Array[Object]] should be === arrayOfObjects
  }

  @Test
  def shouldBeAbleToMarshallObjectMatrixes {
    val marshalled = objectMatrix(matrixOfObjects)
    val unmarshalled = fromValue(marshalled, matrixOfObjects.getClass)

    assertNotNull(unmarshalled)

    unmarshalled.asInstanceOf[Array[Array[Object]]] should be === matrixOfObjects
  }

  @Test
  def shouldConvertManyParametersIntoOneSequence {
    val value: Any = fromValues(List(stringValue("x"), stringValue("a"), stringValue("b")), Array(classOf[String], classOf[Seq[String]]))

    value should be === List("x", List("a", "b").toSeq)
  }
}