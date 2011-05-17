package starling.loopyxl

import starling.utils.ImplicitConversions._

import LoopyXL._
import InvocationValue.Type._
import TypedObject.Type._

import RichInvocationValue._

import scala.collection.JavaConversions._
import collection.immutable.List


object ProtocolBuffers {
  implicit def builderToValue(builder : InvocationValue.Builder) = builder.build

  def doubleValue(d: Double) = invocationValue(DOUBLE_VALUE).setDoubleValue(d)
  def stringValue(s: String) = invocationValue(STRING_VALUE).setStringValue(s)
  def booleanValue(value: Boolean) = stringValue(value.toString.toUpperCase)
  def intValue(i: Int) = stringValue(i.toString)
  def objectValue(o: Object) = invocationValue(OBJECT_VALUE).setObjectValue(typedObject(o))

  def doubleArray(array: Array[Double]) = invocationValue(DOUBLE_ARRAY)
    .update { value => array.foreach(elem => value.addDoubleArray(elem)) }

  def stringArray(array: Array[String]) = invocationValue(STRING_ARRAY)
    .update { value => array.foreach(elem => value.addStringArray(elem)) }

  def booleanArray(array: Array[Boolean]) = stringArray(array.map(value => value.toString.toUpperCase))
  def intArray(array: Array[Int]) = stringArray(array.map(value => value.toString))

  def objectArray(array: Array[Object]) = invocationValue(OBJECT_ARRAY)
    .update { value => array.foreach(elem => value.addObjectArray(typedObject(elem))) }

  def doubleMatrix(matrix: Array[Array[Double]]) = invocationValue(DOUBLE_MATRIX)
    .update { value => matrix.foreach(array => value.addDoubleMatrix(matrixRow(array))) }

  def stringMatrix(matrix: Array[Array[String]]) = invocationValue(STRING_MATRIX)
    .update { value => matrix.foreach(array => value.addStringMatrix(matrixRow(array))) }

  def booleanMatrix(matrix: Array[Array[Boolean]]) = stringMatrix(matrix.map(_.map(_.toString.toUpperCase)))
  def intMatrix(matrix: Array[Array[Int]]) = stringMatrix(matrix.map(_.map(_.toString)))

  def objectMatrix(matrix: Array[Array[Object]]) = invocationValue(OBJECT_MATRIX)
    .update { value => matrix.foreach(array => value.addObjectMatrix(matrixRow(array))) }

  def matrixRow(array: Array[Double]) = DoubleArray.newBuilder.update { it => array.foreach(it.addValues(_)) }
  def matrixRow(array: Array[String]) = StringArray.newBuilder.update { it => array.foreach(it.addValues(_)) }
  def matrixRow(array: Array[Object]) = TypedArray.newBuilder.update { it => array.map(typedObject(_)).foreach(it.addValues(_)) }

  def stringMap(map: Map[String, Object]) = stringSeq(map.toSeq)

  def stringSeq(seq: Seq[(String, Object)]) = invocationValue(OBJECT_MATRIX)
    .update { value => seq.foreach{case (k, v) => value.addObjectMatrix(matrixRow(Array[Object](k, v)))}}

  def fromValue(value: InvocationValue, valueClass: Class[_]): Any = valueClass match {
    case `double`  => value.getDoubleValue
    case `string`  => value.getStringValue
    case `boolean` => value.getBooleanValue()
    case `int`     => value.getIntValue()
    case `objekt`  => value.getObjectValue.parse

    case `doubleArray`  => value.getDoubleArray()
    case `stringArray`  => value.getStringArray()
    case `booleanArray` => value.getBooleanArray()
    case `intArray`     => value.getIntArray()
    case `objectArray`  => value.getObjectArray()

    case `doubleMatrix`  => value.getDoubleMatrix()
    case `stringMatrix`  => value.getStringMatrix()
    case `booleanMatrix` => value.getBooleanMatrix()
    case `intMatrix`     => value.getIntMatrix()
    case `objectMatrix`  => value.getObjectMatrix()

    case `stringMap` => value.getObjectMatrix().map(row => (row(0), row(1))).toMap
  }

  def fromValues(values: List[InvocationValue], classes: Array[Class[_]]) = sequence(values, classes) match {
    case Some(sequence) => nonSequence(values, classes) :+ sequence
    case None => nonSequence(values, classes)
  }

  private def nonSequence(values: List[LoopyXL.InvocationValue], classes: Array[Class[_]]): List[Any] = {
    val nonSequenceClasses = if (endsWithSequence(classes)) classes.init else classes

    values.zip(nonSequenceClasses).map(Function.tupled(fromValue _))
  }

  private def sequence(values: List[InvocationValue], classes: Array[Class[_]]) = if (endsWithSequence(classes)) {
    Some(toSequence(values.drop(classes.size - 1), classes.last).reverse.dropWhile(isEmptyOrNull).reverse.toList)
  } else {
    None
  }

  private def isEmptyOrNull(value: Any) = value match {
    case s:String => s.isEmpty
    case o => o == null
  }

  def endsWithSequence(classes: Array[Class[_]]): Boolean = !classes.isEmpty && classOf[Seq[_]].isAssignableFrom(classes.last)

  private def toSequence(values: List[InvocationValue], klass: Class[_]) = {
    klass match {
      case `stringSeq`  => values.map(_.getStringValue)
      case `doubleSeq`  => values.map(_.getDoubleValue)
      case `intSeq`     => values.map(_.getIntValue)
      case `booleanSeq` => values.map(_.getBooleanValue)
    }
  }

  private def invocationValue(valueType: InvocationValue.Type) = InvocationValue.newBuilder.setType(valueType)

  private def typedObject(o: Object) = TypedObject.newBuilder()
    .update { typedObject => o match {
      case d:java.lang.Double => { typedObject.setType(DOUBLE); typedObject.setDoubleValue(d.doubleValue) }
      case s:String           => { typedObject.setType(STRING); typedObject.setStringValue(s)             }
    }}

  private val double = classOf[Double]
  private val string = classOf[String]
  private val int = classOf[Int]
  private val boolean = classOf[Boolean]
  private val objekt = classOf[Object]

  private val doubleArray = classOf[Array[Double]]
  private val stringArray = classOf[Array[String]]
  private val intArray = classOf[Array[Int]]
  private val booleanArray = classOf[Array[Boolean]]
  private val objectArray = classOf[Array[Object]]

  private val doubleMatrix = classOf[Array[Array[Double]]]
  private val stringMatrix = classOf[Array[Array[String]]]
  private val intMatrix = classOf[Array[Array[Int]]]
  private val booleanMatrix: Class[Array[Array[Boolean]]] = classOf[Array[Array[Boolean]]]
  private val objectMatrix = classOf[Array[Array[Object]]]

  private val stringMap = classOf[Map[String, Object]]

  private val stringSeq = classOf[Seq[String]]
  private val doubleSeq = classOf[Seq[Double]]
  private val intSeq = classOf[Seq[Int]]
  private val booleanSeq = classOf[Seq[Boolean]]
}