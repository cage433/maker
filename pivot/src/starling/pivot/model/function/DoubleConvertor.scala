package starling.pivot.model.function

/**
 * User: brian
 * Date: 26-Nov-2009
 * Copyright (c) Trafigura 2009
 */

trait DoubleConvertor {
  // get a value as a double (for maths functions)
  def asDouble(o: Any): Double = {
    if (o == null) {
      0.0D
    } else if (o.isInstanceOf[Float]) {
      o.asInstanceOf[Float].toDouble
    } else if (o.isInstanceOf[Double]) {
      o.asInstanceOf[Double]
    } else if (o.isInstanceOf[Int]) {
      o.asInstanceOf[Int].doubleValue
    } else if (o.isInstanceOf[Long]) {
      o.asInstanceOf[Long].doubleValue
    } else if (o.isInstanceOf[java.lang.Float]) {
      o.asInstanceOf[Float].toDouble
    } else if (o.isInstanceOf[java.lang.Double]) {
      o.asInstanceOf[Double]
    } else if (o.isInstanceOf[java.lang.Integer]) {
      o.asInstanceOf[Int].doubleValue
    } else if (o.isInstanceOf[java.lang.Long]) {
      o.asInstanceOf[Long].doubleValue
    } else {
      0.0D
    }
  }
}
