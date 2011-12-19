package starling.marketdata

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import starling.daterange.Day, Day._
import starling.pivot.controller.{TreePivotFilterNode, TreePivotFilter}
import starling.pivot.LabeledFilterSelection
import starling.utils.ImplicitConversions._
import collection.IterableView


class MarketDataPivotTableDataSourceTests extends WordSpec with ShouldMatchers {
  import MarketDataPivotTableDataSource._

  "observation day field sorts reverse chronologically" in {
    observationDayField.createFilter(List(13 Dec 2011, 15 Dec 2011, 14 Dec 2011)).toStream.toList.filterCast[Day] should be ===
      List(15 Dec 2011, 14 Dec 2011, 13 Dec 2011)
  }

  "observation day field groups days into 'Latest' and 'Historic'" in {
    val daysInDecember = ((1 Dec 2011) upto (10 Dec 2011)).toList
    val reversed = daysInDecember.reverse

    observationDayField.createFilter(daysInDecember) should be ===
      TreePivotFilter(TreePivotFilterNode(LabeledFilterSelection("All"), List(
        TreePivotFilterNode(LabeledFilterSelection("Latest"), nodes(reversed.take(5))),
        TreePivotFilterNode(LabeledFilterSelection("Historic"), nodes(reversed.drop(5)))
      )))
  }

  private def nodes(days: Iterable[Day]): scala.List[TreePivotFilterNode] = days.toList.map(TreePivotFilterNode(_, Nil))
}