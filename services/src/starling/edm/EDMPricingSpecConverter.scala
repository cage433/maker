package starling.edm

import com.trafigura.tradinghub.support.GUID
import starling.market.SingleIndex
import starling.daterange.Day
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.tradecapture.internal.refinedmetal.{Metal, Market}
import starling.instrument._
import physical._
import starling.edm.EDMConversions._
import starling.quantity.{UOM, Quantity}
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * Created by IntelliJ IDEA.
 * User: louis
 * Date: 23/06/11
 * Time: 11:40
 * To change this template use File | Settings | File Templates.
 */

case class EDMPricingSpecConverter(metal : Metal, exchanges : Map[GUID, Market]) {
  def indexFromMarket(exchangeGUID : GUID) : SingleIndex = {
    if (!exchanges.contains(exchangeGUID)){
      exchanges.keySet.foreach(println)
      println(exchangeGUID)
    }

    RefinedTacticalRefDataConversions.guessAtIndex(exchanges(exchangeGUID), metal)
  }

  def fromEdmPricingSpec(deliveryQuantity : Quantity, edmPricingSpec : EDMPricingSpec) : TitanPricingSpec = {
    edmPricingSpec match {
      case spec : EDMMonthAveragePricingSpec => {
        MonthAveragePricingSpec(
          indexFromMarket(spec.market),
          Day.fromJodaDate(spec.qpMonth).containingMonth,
          spec.premium
        )
      }
      case spec : EDMPartAvePrcSpec => {
        val dayQuantities = spec.dayQtyMap.map{
          case dayQty => Day.fromJodaDate(dayQty.date) -> fromTitanQuantity(dayQty.quantity)
        }.toMap
        val totalQuantity = dayQuantities.map(_._2).sum
        PartialAveragePricingSpec(
          indexFromMarket(spec.market),
          dayQuantities.map{
            case (day, qty) => day -> (qty / totalQuantity).value
          },
          spec.premium
        )

      }
      case spec : EDMOptPricingSpec => {
        OptionalPricingSpec(
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
           spec.fixations.map{
             case fixation =>
               val fraction = (fromTitanQuantity(fixation.fixedQuantity) / deliveryQuantity).checkedValue(UOM.NULL)
               UnknownPricingFixation(fraction, fromTitanQuantity(fixation.observedPrice))
           },
           declarationBy,
           spec.premium
        )
      }
      case spec : EDMFixedPricingSpec => {
        // Reasonable guess - The settlement day should live in trade management but doesn't yet
        val settlementDay = spec.comps.map{comp => Day.fromLocalDate(comp.date)}.max.addWeekdays(2)
        FixedPricingSpec(
          settlementDay,
          spec.comps.map{
            case comp => {
              val fraction = (fromTitanQuantity(comp.quantity) / deliveryQuantity).checkedValue(UOM.NULL)
              (fraction, fromTitanQuantity(comp.price))
            }
          }
        )
      }

      case _ => throw new Exception("Unrecognised spec " + edmPricingSpec)
    }
  }

}