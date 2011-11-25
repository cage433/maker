package starling.marketdata

import org.scalatest.matchers.ShouldMatchers
import starling.utils.StarlingSpec
import starling.utils.ImplicitConversions._


class MarketDataTypesTests extends StarlingSpec with ShouldMatchers {
  new MarketDataTypes(ReferenceDataLookup.Null).types.foreach(mdt => mdt.getClass.getSimpleName should {
    "have mutually exclusive value, extended key, value key & derived fields" in {
      (mdt.valueFields.pairWith("Value") ++
       mdt.extendedKeyFields.pairWith("Extended Key") ++
       mdt.valueKeyFields.pairWith("Value Key") ++
       mdt.derivedFields.pairWith("Derived")).toMultiMap.filterValues(_.size > 1) should be === Map()
    }
  })
}