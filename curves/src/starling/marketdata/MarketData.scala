package starling.marketdata

import starling.pivot.{Row, Field}


case class MarketDataRows(rows:List[Map[Field,Any]]) {
  def size = rows.size
  def toMarketData[MDT <: MarketDataType](dataType: MDT): MDT#dataType = dataType.createValue(rows.map(Row(_)))
}


trait MarketData {
  def marshall: Object = this
  def size: Int
}

