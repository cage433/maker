package starling.loopyxl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.testng.TestNGSuite

import org.testng.annotations.Test
import java.lang.String
import collection.mutable.ListBuffer
import starling.utils.ImplicitConversions._
import ProtocolBuffers._


class MethodOwnerTest extends TestNGSuite with ShouldMatchers {
  val invalidTypes = new ListBuffer[AnyRef]
  val owner = {
    new MethodOwner(new OwnerWithMethodsUsingAllValidInputTypes) {
      protected override def logInvalid(method: DynamicMethod) = invalidTypes ++= method.unmarshallable
    }
  }

  @Test
  def shouldLogInvalidMethods {
    owner.dynamicMethods

    invalidTypes should be === Nil
  }

  @Test def shouldBeAbleToDynamicallyInvokeMethodContainingSequence {
    class SequenceMethodOwner {
      @ExcelMethod
      def curveValue(measure: String, params: Map[String, AnyRef], filters : AnyRef*): String = {
        "measure: %s\nparams: %s\nfilters: %s" % (measure, params, filters)
      }
    }

    val owner = new MethodOwner(new SequenceMethodOwner)

    owner.dynamicMethods.size should be === 1

    val dynamicMethod = owner.dynamicMethods(0)

    dynamicMethod.invoke(List(
      stringValue("Price"),
      objectMatrix(Array(Array("Pricing Group", "Barry Eckstein"),
                         Array("Day", new java.lang.Double(40618.0)),
                         Array("Rule", "Default"))),
      stringValue("Market"), stringValue("LME Copper"),
      stringValue("Period"), stringValue("16Mar2011")
    )).getStringValue should be === "measure: Price\n" +
                                    "params: Map(Pricing Group -> Barry Eckstein, Day -> 40618.0, Rule -> Default)\n" +
                                    "filters: List(Market, LME Copper, Period, 16Mar2011)"
  }

  class OwnerWithMethodsUsingAllValidInputTypes {
    @ExcelMethod
    def stringMethod(string: String, stringArray: Array[String], stringMatrix: Array[Array[String]]): String = string

    @ExcelMethod
    def intMethod(i: Int, intArray: Array[Int], intMatrix: Array[Array[Int]]): Int = i

    @ExcelMethod
    def doubleMethod(d: Double, doubleArray: Array[Double], doubleMatrix: Array[Array[Double]]): Double = d

    @ExcelMethod
    def objectMethod(o: Object, objectArray: Array[Object], objectMatrix: Array[Array[Object]]): Object = o

    @ExcelMethod
    def mapMethod(m: Map[String, String]): Int = m.size

    @ExcelMethod
    def sequenceOfStringsMethod(seq: String*): Int = 0

    @ExcelMethod
    def sequenceOfIntsMethod(seq: Int*): Int = 0

    @ExcelMethod
    def sequenceOfDoublesMethod(seq: Double*): Int = 0

    @ExcelMethod
    def sequenceOfObjectsMethod(seq: Object*): Int = 0
  }
}