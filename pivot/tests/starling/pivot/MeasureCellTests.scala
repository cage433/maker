package starling.pivot

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import starling.quantity.{UOM, Quantity}

class MeasureCellTests extends WordSpec with ShouldMatchers {
  "Should be almost zero when containing zero doubles, quantities, etc." in {
    List(measureCell(0.0), measureCell(Quantity(0.0, UOM.SCALAR))).filterNot(_.isAlmoseZero) should be === Nil
  }

  private def measureCell(value: Any) = MeasureCell(Some(value), EditableCellState.Normal)
}