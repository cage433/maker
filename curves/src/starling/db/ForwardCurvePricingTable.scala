package starling.db



sealed class ForwardCurvePricingTable(val tableName : String) {
  override def hashCode = tableName.hashCode
  override def equals(p1: Any) = p1 match {
    case f : ForwardCurvePricingTable => this.tableName == f.tableName
    case _ => false
  }
  override def toString = tableName
}

case object MetalsPriceTable extends ForwardCurvePricingTable("ForwardCurvesMetals.dbo.tblOutputPrices")
case object NonMetalsPriceTable extends ForwardCurvePricingTable("ForwardCurves.dbo.tblOutputPrices")
case object NonMetalsInputPriceTable extends ForwardCurvePricingTable("ForwardCurves.dbo.tblInputPrices")
case object NonMetalsCurvesTable extends ForwardCurvePricingTable("ForwardCurves.dbo.tblCurves")

/**
 * For CME Eurodollar futures, which are a market as they need holidays, but don't require a price table
 */
// TODO [17 Mar 2010] refactor the market class to avoid this
case object NoPriceTable extends ForwardCurvePricingTable("This market has no associated price table")
