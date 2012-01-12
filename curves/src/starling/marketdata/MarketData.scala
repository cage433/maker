package starling.marketdata

import starling.pivot.Field

case class MarketDataRows(rows:List[Map[Field,Any]]) {
  def size = rows.size
}


trait MarketData {
  def marshall: Object = this
  def size: Int
}

