package starling.models

import starling.quantity.Quantity

trait OptionValuation {

  def value(callOrPut: CallOrPut, F: Double, K: Double, sigma: Double, r: Double, T: Double): Double

  def value(callOrPut: CallOrPut, F: Quantity, K: Quantity, sigma: Double, r: Double, T: Double): Quantity = {
    assert(F.uom == K.uom, "Unit mismatch")
    Quantity(value(callOrPut, F.value, K.value, sigma, r, T), F.uom)
  }
}