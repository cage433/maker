package starling.maths

import cern.colt.matrix.{DoubleMatrix1D => Vector}


/**
	Code converted from Numerical Recipes in C
*/
class CubicSpline(x : Array[Double], y : Array[Double], yp1 : Option[Double], ypn : Option[Double]){
  val n = x.size
  require(n > 0)
  require(x.size == y.size)

  val y2 = Array.fill(n)(0.0)
  if (n > 1){
    val u = Array.fill(n)(0.0)
    yp1 match {
      case None => {
        y2(0) = 0;	
        u(0) = 0;
      }
      case Some(yp1_) => {
        y2(0) = -0.5;
        u(0) =(3.0 /(x(1) - x(0)))*((y(1) - y(0))/(x(1) - x(0))- yp1_);
      }
    }

		for(i <- 1 until n - 1){ 
			val sig =(x(i) - x(i - 1))/(x(i + 1) - x(i - 1));
			val p = sig * y2(i - 1) + 2;
			y2(i) =(sig - 1)/ p;
			u(i) =(y(i + 1) - y(i))/(x(i + 1) - x(i))-(y(i) - y(i - 1))/(x(i) - x(i - 1));
			u(i) =(6.0 * u(i) /(x(i + 1) - x(i - 1))- sig * u(i - 1))/ p;
		}
		val (qn , un) = ypn match {
      case None => (0.0, 0.0)
      case Some(ypn_) => 
        (0.5,
         (3.0 /(x(n - 1) - x(n - 2)))*(ypn_ -(y(n - 1) - y (n - 2))/(x(n - 1) - x(n - 2)))
        )
    }
		y2(n - 1) =(un - qn * u(n - 2))/(qn * y2(n - 2) + 1.0);
				
    for (i <- n - 2 to 0 by -1){
			y2(i) = y2(i) * y2(i + 1) + u(i);
		}
  }
	/**
		This creates a natural cubic spline with zero second derivative at the endpoints
	*/
	def this(x : Array[Double], y : Array[Double]){
		this(x, y, None, None) 
	}
  def this(x : Vector, y : Vector){
    this(x.toArray, y.toArray, None, None) 
  }
	def apply(x0 : Double) : Double = {
		if(x.length == 1){
			return y(0);
		}
		
		var klo = 0;
		var khi = x.length - 1;
		var k = 0;
		while(khi - klo > 1){
			k =(khi + klo)>> 1;
			if(x(k) > x0){
				khi = k;
			} else {
				klo = k;
			}
		}
		val h = x(khi) - x(klo);
		val a =(x(khi) - x0)/ h;
		val b =(x0 - x(klo))/ h;
		return a * y(klo) + b * y(khi) +((a * a * a - a)*y2(klo) +(b * b * b - b)* y2(khi))* h * h / 6.0;
		
	}
}
