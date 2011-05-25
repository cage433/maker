package starling.services

import starling.quantity.UOM
import collection.SortedMap
import starling.daterange.DateRange

/**
 * decorates forward curve data with extra information.
 */
case class ForwardCurveInfo(
        prices : SortedMap[DateRange, Double],
        uom : UOM,
        id : Int,
        leg1Name : String,
        leg2Name : Option[String])