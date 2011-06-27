package starling.edm

import com.trafigura.tradinghub.support.GUID
import starling.market.SingleIndex
import starling.daterange.Day
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.tradecapture.internal.refinedmetal.{Metal, Market}
import starling.instrument._
import starling.edm.EDMConversions._
import starling.quantity.Quantity

/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 23/06/11
 * Time: 11:40
 * To change this template use File | Settings | File Templates.
 */

case class EDMPricingSpecConverter(metal : Metal, exchanges : Map[GUID, Market]) {
  def indexFromMarket(exchangeGUID : GUID) : SingleIndex = RefinedTacticalRefDataConversions.guessAtIndex(exchanges(exchangeGUID), metal)

  def fromEdmPricingSpec(deliveryQuantity : Quantity, edmPricingSpec : EDMPricingSpec) : PricingSpec = {
    edmPricingSpec match {
      case spec : EDMMonthAveragePricingSpec => {
        MonthAveragePricingSpec(
          deliveryQuantity,
          indexFromMarket(spec.market),
          Day.fromJodaDate(spec.qpMonth).containingMonth,
          spec.premium
        )
      }
      case spec : EDMPartAvePrcSpec => {
        val dayQuantities = spec.dayQtyMap.map{
          case dayQty => Day.fromJodaDate(dayQty.date) -> fromQuantityE(dayQty.quantity)
        }.toMap
        val totalQuantity = dayQuantities.map(_._2).sum
        PartialAveragePricingSpec(
          deliveryQuantity,
          indexFromMarket(spec.market),
          dayQuantities.map{
            case (day, qty) => day -> (qty / totalQuantity).value
          },
          spec.premium
        )

      }
      case spec : EDMOptPricingSpec => {
        OptionalPricingSpec(
          deliveryQuantity,
          spec.choices.map(fromEdmPricingSpec(deliveryQuantity, _)),
          Day.fromJodaDate(spec.declarationBy),
          if (spec.chosenSpec == null)
            None
          else
            Some(fromEdmPricingSpec(deliveryQuantity, spec.chosenSpec))
        )
      }
      case spec : EDMWtdPricingSpec => {
        WeightedPricingSpec(
          deliveryQuantity,
          spec.wtdSpecs.map{
            case weightedSpec =>
               (weightedSpec.weight, fromEdmPricingSpec(deliveryQuantity * weightedSpec.weight, weightedSpec.pricingSpec))
          }
        )
      }
      case spec : EDMUnkPricingSpec => {
        val qpMonth = Day.fromJodaDate(spec.qpMonth).containingMonth
        val index: SingleIndex = indexFromMarket(spec.market)
        val declarationBy: Day = if (spec.declarationBy == null) qpMonth.lastDay.thisOrPreviousBusinessDay(index.businessCalendar) else Day.fromJodaDate(spec.declarationBy)
        UnknownPricingSpecification(
           index,
           qpMonth,
           deliveryQuantity,
           spec.fixations.map{
             case fixation =>
              UnknownPricingFixation(fromQuantityE(fixation.fixedQuantity), fromQuantityE(fixation.observedPrice))
           },
           declarationBy,
           spec.premium
        )
      }
      case spec : EDMFixedPricingSpec => {
        FixedPricingSpec(
          deliveryQuantity,
          spec.comps.map{
            case comp =>
              (fromQuantityE(comp.quantity), fromQuantityE(comp.price))
          }
        )
      }

      case _ => throw new Exception("Unrecognised spec " + edmPricingSpec)
    }
  }

}