package starling.titan

import com.trafigura.tradinghub.support.GUID
import starling.daterange.Day
import com.trafigura.edm.physicaltradespecs.{PricingSpecification, FixedPricingSpecification, MonthAveragePricingSpecification, PartialAveragePricingSpecification,OptionalPricingSpecification,WeightedPricingSpecification,UnknownPricingSpecification => UNKPricingSpecification}
import com.trafigura.edm.physicaltradespecs.{CashAveragePricingSpecificationIndex, ThreeMonthAveragePricingSpecificationIndex, LowestOfFourAveragePricingSpecificationIndex, AverageOfFourAveragePricingSpecificationIndex, MaxSettlementAveragePricingSpecificationIndex, CashUnknownPricingSpecificationIndex, ThreeMonthUnknownPricingSpecificationIndex, LowestOfFourUnknownPricingSpecificationIndex, AverageOfFourUnknownPricingSpecificationIndex, MaxSettlementUnknownPricingSpecificationIndex, AveragePricingSpecificationIndexEnum, UnknownPricingSpecificationIndexEnum}
import com.trafigura.tradecapture.internal.refinedmetal.{Metal, Market}
import starling.instrument._
import physical._
import starling.quantity.{UOM, Quantity}
import starling.titan.EDMConversions._
import collection.immutable.{TreeMap, Map}
import starling.market.IndexWithKnownPrice


trait TitanIndexName {
  def name : String
}

case object CashIndex extends TitanIndexName { val name = "Cash"} 
case object ThreeMonthIndex extends TitanIndexName { val name = "Three Month"} 
case object LowestOfFourIndex extends TitanIndexName { val name = "Lowest of Four"} 
case object AverageOfFourIndex extends TitanIndexName { val name = "Average of Four"} 
case object Ave4MaxSettIndex extends TitanIndexName { val name = "Max Settlement"} 

object TitanIndexName {
  val cashAverageIndex = CashAveragePricingSpecificationIndex 
  val threeMonthAverageIndex = ThreeMonthAveragePricingSpecificationIndex 
  val lowestOfFourAverageIndex = LowestOfFourAveragePricingSpecificationIndex 
  val averageOfFourAverageIndex = AverageOfFourAveragePricingSpecificationIndex 
  val maxSettlementAverageIndex = MaxSettlementAveragePricingSpecificationIndex 

  implicit def fromAveragePricingSpecificationIndexEnum (index : AveragePricingSpecificationIndexEnum) : TitanIndexName = {
    index match {
      case `cashAverageIndex` => CashIndex
      case `threeMonthAverageIndex` => ThreeMonthIndex
      case `lowestOfFourAverageIndex` => LowestOfFourIndex
      case `averageOfFourAverageIndex` => AverageOfFourIndex
      case `maxSettlementAverageIndex` => Ave4MaxSettIndex
    }
  }

  val cashUnknownIndex = CashUnknownPricingSpecificationIndex 
  val threeMonthUnknownIndex = ThreeMonthUnknownPricingSpecificationIndex 
  val lowestOfFourUnknownIndex = LowestOfFourUnknownPricingSpecificationIndex 
  val averageOfFourUnknownIndex = AverageOfFourUnknownPricingSpecificationIndex 
  val maxSettlementUnknownIndex = MaxSettlementUnknownPricingSpecificationIndex 

  implicit def fromAveragePricingSpecificationIndexEnum (index : UnknownPricingSpecificationIndexEnum) : TitanIndexName = {
    index match {
      case `cashUnknownIndex` => CashIndex
      case `threeMonthUnknownIndex` => ThreeMonthIndex
      case `lowestOfFourUnknownIndex` => LowestOfFourIndex
      case `averageOfFourUnknownIndex` => AverageOfFourIndex
      case `maxSettlementUnknownIndex` => Ave4MaxSettIndex
    }
  }
}

case class EDMPricingSpecConverter(metal : Metal, exchanges : String => Market) {
  import TitanIndexName._
  def getIndex(exchangeID : String, indexName : TitanIndexName) : IndexWithKnownPrice = {
    RefinedTacticalRefDataConversions.index(exchanges(exchangeID), metal, indexName)
  }

  def fromEdmPricingSpec(deliveryDay : Day, deliveryQuantity : Quantity, edmPricingSpec : PricingSpecification) : TitanPricingSpec = {
    edmPricingSpec match {
      case spec : MonthAveragePricingSpecification => {
        MonthAveragePricingSpec(
          getIndex(spec.market, spec.index),
          Day.fromJodaDate(spec.qpMonth).containingMonth,
          spec.premium
        )
      }
      case spec : PartialAveragePricingSpecification => {
        val dayQuantities = spec.dayQtyMap.map{
          case dayQty => Day.fromJodaDate(dayQty.date) -> fromTitanQuantity(dayQty.quantity)
        }.toMap
        val totalQuantity = dayQuantities.map(_._2).sum

        val sortedDayFractions = new TreeMap[Day, Double]() ++ dayQuantities.map{
          case (day, qty) => day -> (qty / totalQuantity).value
        }

        val index = getIndex(spec.market, spec.index)
        // Some of the day fractions are incorrect in titan - using non business days
        val (validDayFractions, invalidDayFractions) = sortedDayFractions.partition{case (day, _) => index.isObservationDay(day)}
        val invalidAmount = invalidDayFractions.values.sum

        PartialAveragePricingSpec(
          index,
          new TreeMap[Day, Double]() ++ validDayFractions.mapValues(_ + invalidAmount / validDayFractions.size),
          spec.premium
        )

      }
      case spec : OptionalPricingSpecification => {
        OptionalPricingSpec(
          spec.choices.map(fromEdmPricingSpec(deliveryDay, deliveryQuantity, _)),
          Day.fromJodaDate(spec.declarationBy),
          if (spec.chosenSpec == null)
            None
          else
            Some(fromEdmPricingSpec(deliveryDay, deliveryQuantity, spec.chosenSpec))
        )
      }
      case spec : WeightedPricingSpecification => {
        WeightedPricingSpec(
          spec.wtdSpecs.map{
            case weightedSpec =>
               (weightedSpec.weight, fromEdmPricingSpec(deliveryDay, deliveryQuantity * weightedSpec.weight, weightedSpec.pricingSpec))
          }
        )
      }
      case spec : UNKPricingSpecification => {
        val qpMonth = Day.fromJodaDate(spec.qpMonth).containingMonth
        val index: IndexWithKnownPrice = getIndex(spec.market, spec.index)
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
      case spec : FixedPricingSpecification => {
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
