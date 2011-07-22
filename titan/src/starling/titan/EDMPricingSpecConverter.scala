package starling.titan

import com.trafigura.tradinghub.support.GUID
import starling.market.SingleIndex
import starling.daterange.Day
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.tradecapture.internal.refinedmetal.{Metal, Market}
import starling.instrument._
import physical._
import starling.quantity.{UOM, Quantity}
import starling.titan.EDMConversions._
import collection.immutable.{TreeMap, Map}

case class EDMPricingSpecConverter(metal : Metal, exchanges : Map[GUID, Market]) {
  def indexFromMarket(exchangeGUID : GUID) : SingleIndex = {
    if (!exchanges.contains(exchangeGUID)){
      exchanges.keySet.foreach(println)
      println(exchangeGUID)
    }

    RefinedTacticalRefDataConversions.guessAtIndex(exchanges(exchangeGUID), metal)
  }

  def fromEdmPricingSpec(deliveryDay : Day, deliveryQuantity : Quantity, edmPricingSpec : EDMPricingSpec) : TitanPricingSpec = {
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

        val sortedDayFractions = new TreeMap[Day, Double]() ++ dayQuantities.map{
          case (day, qty) => day -> (qty / totalQuantity).value
        }

        val index = indexFromMarket(spec.market)
        // Some of the day fractions are incorrect in titan - using non business days
        val (validDayFractions, invalidDayFractions) = sortedDayFractions.partition{case (day, _) => index.isObservationDay(day)}
        val invalidAmount = invalidDayFractions.values.sum

        PartialAveragePricingSpec(
          index,
          new TreeMap[Day, Double]() ++ validDayFractions.mapValues(_ + invalidAmount / validDayFractions.size),
          spec.premium
        )

      }
      case spec : EDMOptPricingSpec => {
        OptionalPricingSpec(
          spec.choices.map(fromEdmPricingSpec(deliveryDay, deliveryQuantity, _)),
          Day.fromJodaDate(spec.declarationBy),
          if (spec.chosenSpec == null)
            None
          else
            Some(fromEdmPricingSpec(deliveryDay, deliveryQuantity, spec.chosenSpec))
        )
      }
      case spec : EDMWtdPricingSpec => {
        WeightedPricingSpec(
          spec.wtdSpecs.map{
            case weightedSpec =>
               (weightedSpec.weight, fromEdmPricingSpec(deliveryDay, deliveryQuantity * weightedSpec.weight, weightedSpec.pricingSpec))
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
               val fraction = (fromTitanQuantity(fixation.fixedQuantity) / deliveryQuantity).checkedValue(UOM.SCALAR)
               UnknownPricingFixation(fraction, fromTitanQuantity(fixation.observedPrice))
           },
           declarationBy,
           spec.premium
        )
      }
      case spec : EDMFixedPricingSpec => {
        assert(spec.comps.nonEmpty, "Fixed pricing spec with no fixed prices")
        // Reasonable guess - The settlement day should live in trade management but doesn't yet
        val settlementDay = spec.comps.flatMap{comp => if (comp.date == null) None else Some(Day.fromLocalDate(comp.date))}.sortWith(_>_).headOption.getOrElse(deliveryDay).addWeekdays(2)
        FixedPricingSpec(
          settlementDay,
          spec.comps.map{
            case comp => {
              val fraction = (fromTitanQuantity(comp.quantity) / deliveryQuantity).checkedValue(UOM.SCALAR)
              (fraction, fromTitanQuantity(comp.price))
            }
          }
        )
      }

      case _ => throw new Exception("Unrecognised spec " + edmPricingSpec)
    }
  }

}