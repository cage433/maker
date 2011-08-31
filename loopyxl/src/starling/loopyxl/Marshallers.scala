package starling.loopyxl

import java.lang.Class
import collection.immutable.List
import starling.utils.ImplicitConversions._
import LoopyXL._
import ProtocolBuffers._
import scalaz._
import Scalaz._

object Marshallers {
  def unmarshallable(dataTypes : List[Class[_]]): List[Class[_]] = dataTypes.filterNot(contains(_))
  def contains(dataType : Class[_]): Boolean = marshallers.exists(_.dataType == dataType)
  def marshaller(dataType : Class[_]): Marshaller = marshallers.find(_.dataType == dataType).get
  def shortName(dataType : Class[_]): String = marshaller(dataType).shortName

  def shortNames(dataType: Class[_]): List[String] = {
    if (isSequence(dataType)) {
      shortName(classOf[String]).replicate[List](10)
    } else {
      List(shortName(dataType))
    }
  }

  private def isSequence(klazz: Class[_]): Boolean = classOf[Seq[_]].isAssignableFrom(klazz)

  private val marshallers = List(
    Marshaller(classOf[Object],   "object", result => objectValue(result.asInstanceOf[Object])),
    Marshaller(classOf[Double],   "double", result => doubleValue(result.asInstanceOf[Double])),
    Marshaller(classOf[String],   "string", result => stringValue(result.asInstanceOf[String])),
    Marshaller(classOf[Boolean],  "string", result => booleanValue(result.asInstanceOf[Boolean])),
    Marshaller(classOf[Int],      "string", result => intValue(result.asInstanceOf[Int])),

    Marshaller(classOf[Array[Double]],  "double[]", result => doubleArray(result.asInstanceOf[Array[Double]])),
    Marshaller(classOf[Array[String]],  "string[]", result => stringArray(result.asInstanceOf[Array[String]])),
    Marshaller(classOf[Array[Boolean]], "string[]", result => booleanArray(result.asInstanceOf[Array[Boolean]])),
    Marshaller(classOf[Array[Int]],     "string[]", result => intArray(result.asInstanceOf[Array[Int]])),
    Marshaller(classOf[Array[Object]], "object[]", result => objectArray(result.asInstanceOf[Array[Object]])),

    Marshaller(classOf[Array[Array[Double]]],  "double[][]", result => doubleMatrix(result.asInstanceOf[Array[Array[Double]]])),
    Marshaller(classOf[Array[Array[String]]],  "string[][]", result => stringMatrix(result.asInstanceOf[Array[Array[String]]])),
    Marshaller(classOf[Array[Array[Boolean]]], "string[][]", result => booleanMatrix(result.asInstanceOf[Array[Array[Boolean]]])),
    Marshaller(classOf[Array[Array[Int]]],     "string[][]", result => intMatrix(result.asInstanceOf[Array[Array[Int]]])),
    Marshaller(classOf[Array[Array[Object]]],  "object[][]", result => objectMatrix(result.asInstanceOf[Array[Array[Object]]])),

    Marshaller(classOf[Map[String, Object]], "object[][]", result => stringMap(result.asInstanceOf[Map[String, Object]])),
    Marshaller(classOf[Seq[(String, Object)]], "object[][]", result => stringMap(result.asInstanceOf[Map[String, Object]]))
  )
}

case class Marshaller(dataType : Class[_], shortName : String, serialize : Any => InvocationValue)
