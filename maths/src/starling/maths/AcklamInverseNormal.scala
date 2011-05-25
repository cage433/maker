package starling.maths

import cern.colt.function.DoubleFunction
import math.{log, sqrt}

/**
 * Acklam's rational polynomial approximation to the inverse normal cumulative distribution function.
 * 
 * This method gives a highly accurate inverse CDF (to within 1.5e-9 relative) by fitting two rational polynomials
 * in a central and boundary regions. The full method, including analysis and code, is on Acklam's website.
 * 
 * http://home.online.no/~pjacklam/notes/invnorm/
 */
object AcklamInverseNormal extends DoubleFunction {
	
	val X_LOW  = 0.02425D
	val X_HIGH = 1.0D - X_LOW

	// coefficients for the rational polynomial approximations.
	val COEFF_A = Array(
		-3.969683028665376e+01,  
		 2.209460984245205e+02,
		-2.759285104469687e+02,  
		 1.383577518672690e+02,
		-3.066479806614716e+01,  
		 2.506628277459239e+00 
	)

	val COEFF_B = Array(
		-5.447609879822406e+01,  
		 1.615858368580409e+02,
		-1.556989798598866e+02,  
		 6.680131188771972e+01,
		-1.328068155288572e+01 
	)

	val COEFF_C = Array(
		-7.784894002430293e-03, 
		-3.223964580411365e-01,
		-2.400758277161838e+00, 
		-2.549732539343734e+00,
		 4.374664141464968e+00,  
		 2.938163982698783e+00 
	)

	val COEFF_D = Array(
		7.784695709041462e-03,  
		3.224671290700398e-01,
		2.445134137142996e+00,  
		3.754408661907416e+00 
	)

	def apply(x : Double) =  {
		invoke(x)
	}
	
	def invoke(x : Double) = {
		var z = 0.0D
		
		// handle the error case first - the inverse normal function only exists for 0 <= x <= 1.
		if (java.lang.Double.isNaN(x) || x < 0 || x > 1) {
			z = Double.NaN

			// in the lower approximation region:
		} else if( x < X_LOW ) {
			val q  = sqrt(-2*log(x))
			z = (((((COEFF_C(0)*q+COEFF_C(1))*q+COEFF_C(2))*q+COEFF_C(3))*q+COEFF_C(4))*q+COEFF_C(5)) / ((((COEFF_D(0)*q+COEFF_D(1))*q+COEFF_D(2))*q+COEFF_D(3))*q+1)

			// in the upper approximation region
		} else if ( X_HIGH < x ) {
			val q  = sqrt(-2*log(1-x))
			z = -(((((COEFF_C(0)*q+COEFF_C(1))*q+COEFF_C(2))*q+COEFF_C(3))*q+COEFF_C(4))*q+COEFF_C(5)) / ((((COEFF_D(0)*q+COEFF_D(1))*q+COEFF_D(2))*q+COEFF_D(3))*q+1)

			// otherwise we're in the middle bit.
		} else {
			val q = x - 0.5D
			val r = q * q
			z = (((((COEFF_A(0)*r+COEFF_A(1))*r+COEFF_A(2))*r+COEFF_A(3))*r+COEFF_A(4))*r+COEFF_A(5))*q / (((((COEFF_B(0)*r+COEFF_B(1))*r+COEFF_B(2))*r+COEFF_B(3))*r+COEFF_B(4))*r+1)
		}
		
		z
	}

}
