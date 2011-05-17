package starling.maths

/**
 * Applies the heat equation over a single timestep using Crank-Nicholson. Used to value american options.
 *
 * this boils down to solving
 *              V * x = U * d
 * for x, where U and V are tridiagonal matrices
 *
 * Written using double[] rather than Colt matrices otherwise boxing is forced, and this doubles
 * execution time
 */

trait FiniteDifferenceScheme
case object SemiImplicit extends FiniteDifferenceScheme
case object FullyImplicit extends FiniteDifferenceScheme
class HeatDiffuser(dz : Double, dt : Double, n : Int, scheme : FiniteDifferenceScheme = SemiImplicit){
  
  val (a, b) = scheme match {
    case FullyImplicit => 
      ( Array.fill(n)(-1 /(dz * dz)),
        Array.fill(n)(2 / dt + 2 /(dz * dz)) )
    case SemiImplicit => 
      ( Array.fill(n)(-0.5 /(dz * dz)),
        Array.fill(n)(2 / dt + 1 /(dz * dz)) )
  }
  //val a = Array.fill(n)(-0.5 /(dz * dz))
  //val b = Array.fill(n)(2 / dt + 1 /(dz * dz))

  private def multiplyByU(C : Array[Double]) : Array[Double] = {
    val (p1, p2) = scheme match {
      case FullyImplicit => (0.0, 2 / dt)
      case SemiImplicit => (0.5 / (dz * dz), 2 / dt - 1 / (dz * dz))
    }
    //val p1 = (0.5 / (dz * dz))
    //val p2 = (2 / dt - 1 / (dz * dz))
    val result = new Array[Double](n)
    for (i <- 1 until n - 1){
      result(i) =
        p1 * C(i - 1) +
        p2 * C(i) +
        p1 * C(i + 1)
    }
    result
  }

  /**
   * Taken from wikipedia, which no doubt took it from Numerical Recipes
   */
  private def solveUsingThomasAlgorithm(d : Array[Double]) : Array[Double] = {
    val x = new Array[Double](n)
    val c = a.clone
    /* Modify the coefficients. */
    c(0) = c(0) / b(0)  /* Division by zero risk. */ 
    c(0) = c(0) / b(0)  /* Division by zero risk. */
    d(0) = d(0) / b(0)  /* Division by zero would imply a singular matrix. */
    for (i <- 1 until n) {
      val id = 1 / (b(i) - c(i - 1) * a(i))  /* Division by zero risk. */
      c(i) = c(i) * id                           /* Last value calculated is redundant. */
      d(i) = (d(i) - d(i - 1) * a(i)) * id
    }

    /* Now back substitute. */
    x(n - 1) = d(n - 1)
    for (i <- n - 2 to  0 by -1)
      x(i) = d(i) - c(i) * x(i + 1)

    x
  }

  def diffuse(d : Array[Double]) : Array[Double] = {
      solveUsingThomasAlgorithm(multiplyByU(d))
  }
}
