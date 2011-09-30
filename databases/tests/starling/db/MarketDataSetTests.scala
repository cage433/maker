package starling.db

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec


class MarketDataSetTests extends WordSpec with ShouldMatchers {
  "fromName works for all entries" in {
    MarketDataSet.values.foreach(mds => MarketDataSet.fromName(mds.name) should be === mds)
  }

  "fromName works with untrimmed strings" in {
    MarketDataSet.values.foreach(mds => MarketDataSet.fromName(" " + mds.name + " ") should be === mds)
  }
}