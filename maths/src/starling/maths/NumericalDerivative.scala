package starling.maths

/**
 * Creates a function that is the numeric derivative of a given function.
 * Higher order terms can be obtained by chaining calls to differentiate,
 * however numerical errors will increase rapidly
 */
class NumericalDerivative(fn : Double => Double, dx : Double){
  def apply(x : Double) = (fn(x + dx) - fn(x - dx)) / (2 * dx)
  def differentiate = new NumericalDerivative(apply _, dx)
}