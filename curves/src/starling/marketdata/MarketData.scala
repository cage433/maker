package starling.marketdata


trait MarketData {
  def marshall: Object = this
  def size: Option[Int] = None
}

