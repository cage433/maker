package starling.maths


/**
 * Function solver - only intended for unit tests
 */
object BisectSolve {
  // solves x : fn(x) = 0
  def apply(fn : Double => Double, xNeg : Double, xPos : Double, eps : Double) : Double = {

    def improve(xNeg : Double, yNeg : Double, xPos : Double, yPos : Double) : Double = {
      val xMid = (xNeg + xPos) / 2.0
      if ((yPos - yNeg).abs < eps){
        xMid
      } else {
        val yMid = fn(xMid)
        if (yMid > 0)
          improve(xNeg, yNeg, xMid, yMid)
        else
          improve(xMid, yMid, xPos, yPos)
      }
    }

    val yNeg = fn(xNeg)
    val yPos = fn(xPos)
    improve(xNeg, yNeg, xPos, yPos)
  }

  // solves fn(x) = y
  def apply(fn : Double => Double, y : Double, xNeg : Double, xPos : Double, eps : Double) : Double=
    apply({x : Double => fn(x) - y}, xNeg, xPos, eps)
}