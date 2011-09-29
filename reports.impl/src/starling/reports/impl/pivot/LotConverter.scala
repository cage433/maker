package starling.reports.impl.pivot

import starling.quantity.UOM._
import starling.quantity.{Quantity, UOM}
import starling.reports.impl.pivot.greeks.GreekValues
import starling.pivot.PivotQuantity

object LotConverter {
  def convertThetas(rows : List[Theta]) : List[Theta] = rows.map{ theta =>
    theta.copy(
      theta = convert(theta.theta)
    )
  }
  def convertGreeks(rows : List[GreekValues]) : List[GreekValues] = rows.map{ greek =>
    greek.copy(
      position = convert(greek.position),
      gamma = convert(greek.gamma),
      deltaBleed = convert(greek.deltaBleed),
      gammaBleed = convert(greek.gammaBleed),
      vega = convert(greek.vega)
    )
  }

  def convert(PivotQuantity : PivotQuantity) : PivotQuantity = PivotQuantity.convertQuantity(convert)
  def convert(q : Quantity) : Quantity = (q in convert(q.uom)).get
  def convert(uom: UOM): UOM = uom match {
    case `BBL` => K_BBL
    case `MT` => K_MT
    case `M3` => C_M3
    case `GAL` => K_GAL
    case `GammaUOM`  => (K_BBL ^2) / USD
    case other => other
  }

  private val GammaUOM = (BBL ^2) / USD
}