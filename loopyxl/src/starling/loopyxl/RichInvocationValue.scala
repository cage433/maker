package starling.loopyxl

import LoopyXL._
import InvocationValue.Type._

import scala.collection.JavaConversions._
import starling.utils.ImplicitConversions._


object RichInvocationValue {
  implicit def enrich(value: InvocationValue) = new RichInvocationValue(value)
  implicit def enrich(value: TypedObject) = new {
    def parse: Object = value.getType match {
      case TypedObject.Type.DOUBLE => value.getDoubleValue.asInstanceOf[Object]
      case TypedObject.Type.STRING => value.getStringValue
    }
  }

  class RichInvocationValue(value : InvocationValue) {
    def getBooleanValue() : Boolean = parseBoolean(value.getStringValue)
    def getIntValue() : Int = Integer.parseInt(value.getStringValue)

    def getDoubleArray() : Array[Double] = value.getDoubleArrayList.map(d => d.doubleValue).toArray
    def getStringArray() : Array[String] = value.getStringArrayList.toList.toArray
    def getIntArray() : Array[Int] = value.getStringArrayList.map(Integer.parseInt(_)).toArray
    def getBooleanArray() : Array[Boolean] = value.getStringArrayList.map(parseBoolean(_)).toArray
    def getObjectArray() : Array[Object] = value.getObjectArrayList.map(_.parse).toArray

    def getDoubleMatrix() : Array[Array[Double]] =
      value.getDoubleMatrixList.map(array => array.getValuesList.map(d => d.doubleValue).toArray).toArray

    def getStringMatrix() : Array[Array[String]] =
      value.getStringMatrixList.map(array => array.getValuesList.toList.toArray).toArray

    def getIntMatrix() : Array[Array[Int]] =
      value.getStringMatrixList.map(array => array.getValuesList.map(Integer.parseInt(_)).toArray).toArray

    def getBooleanMatrix() : Array[Array[Boolean]] =
      value.getStringMatrixList.map(array => array.getValuesList.map(parseBoolean(_)).toArray).toArray

    def getObjectMatrix() : Array[Array[Object]] =
      value.getObjectMatrixList.map(array => array.getValuesList.map(_.parse).toArray).toArray

    private def parseBoolean(s : String) = java.lang.Boolean.parseBoolean(s)
  }
}