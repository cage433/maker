package starling.reports.pivot

import starling.quantity.UOM._
import starling.quantity.{Quantity, UOM}
import starling.reports.pivot.greeks.GreekValues
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
    case `USD` => USD * 1000
    case `GammaUOM`  => GammaUOM * 1000
    case `GAL` => GAL * 1000
    case other => other
  }

  private val GammaUOM = (BBL ^2) / USD
}
