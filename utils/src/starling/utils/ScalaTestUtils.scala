package starling.utils

import ImplicitConversions._

object ScalaTestUtils {

  /** Utility to produce TestNG DataProvider args from a var list of tuples
   */

  def constructArgs(xss : Product*) = {
    val arrs = Array.ofDim[Object](xss.length, xss.maximum(_.productArity))
    for {
      (arr, xs) <- arrs.iterator zip xss.iterator
      i <- 0 until xs.productArity
    } arr(i) = (xs.productElement(i)).asInstanceOf[Object]
    arrs
  }
  /** Utility to construct single parameter data provider args from a sequence
   */
  def constructDataProviderArgs(xss : Seq[AnyRef]) : Array[Array[Object]] = {
    xss.toList.map{x => List[Object](x).toArray}.toArray
  }

//  /** Assert that UOMs match and that values are within tolerance
//   */
//  def assertQtyEquals(actual : Quantity, expected : Quantity, tol : Double){
//    assertEquals(actual.value, expected.value, tol)
//    assertEquals(actual.uom, expected.uom)
//  }
}
