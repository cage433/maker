package starling.loopyxl

import util.Random

class MethodImplementation {
  private val random = new Random

  @ExcelMethod
  def I(input: Int): Int = random.nextInt

  @ExcelMethod
  def IEcho(input: Int): Int = input

  @ExcelMethod
  def D(input: Double): Double = random.nextDouble

  @ExcelMethod
  def S(input: String): String = input + ": back at ya"

  @ExcelMethod
  def B(input: Boolean) : Boolean = input

  @ExcelMethod
  def AD(values: Array[Double]) : Double = values.sum

  @ExcelMethod
  def ADAD(left : Array[Double], right : Array[Double]) : Double = AD(left) + AD(right)

  @ExcelMethod
  def ADADD(left : Array[Double], right : Array[Double], d : Double) : Double = AD(left) + AD(right) + d

  @ExcelMethod
  def ADDAD(left : Array[Double], d : Double, right : Array[Double]) : Double = AD(left) + d + AD(right)

  @ExcelMethod
  def MD(values : Array[Array[Double]]) : Double = values.map(_.sum).sum

  @ExcelMethod
  def MDMD(left : Array[Array[Double]], right : Array[Array[Double]]) : Double = MD(left) + MD(right)

  @ExcelMethod
  def MDEcho(values : Array[Array[Double]]) = values

  @ExcelMethod
  def DAD(d : Double, ds : Array[Double]) = ds.sum + d

  @ExcelMethod
  def DADD(d1 : Double, ds : Array[Double], d2 : Double) = ds.sum + d1 + d2

  @ExcelMethod
  def AO(oa : Array[Object]) : Int = oa.length

  @ExcelMethod
  def MO(om : Array[Array[Object]]) : Int = om.map(_.length).sum

  @ExcelMethod
  def AS(values: Array[String]): String = "Whatever"

  @ExcelMethod
  def StringMatrix(m: Array[Array[String]]): Double = 345.789

  @ExcelMethod
  def StringMap(sm: Map[String, Object]): Int = sm.size

  @ExcelMethod
  def StringSequence(ss: String*): Int = 3

  @ExcelMethod
  def Sequence(seq: (String, Object)*): Int = {
    seq.foreach{case (a, b) => println(a + " " + b)}
    3
  }
}