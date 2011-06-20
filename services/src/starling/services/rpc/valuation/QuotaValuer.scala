package starling.services.rpc.valuation

import starling.quantity.Quantity
import starling.daterange.{DayOfWeek, Day, Month}
import com.trafigura.tradinghub.support.GUID
import starling.edm.EDMConversions._
import com.trafigura.edm.physicaltradespecs._
import com.trafigura.edm.materialspecification.CommoditySpec
import starling.market.{Aluminium, Commodity, SingleIndex}
import starling.services.Server
import java.lang.Object
import starling.db.{NormalMarketDataReader, SnapshotID}
import starling.gui.api.{MarketDataIdentifier, PricingGroup, SnapshotIDLabel}
import starling.curves.{ClosesEnvironmentRule, Environment}

trait PricingSpec{
  def price(env : Environment) : Quantity
}

case class MonthAveragePricingSpec(index : SingleIndex, month : Month, premium : Quantity) extends PricingSpec{
  def price(env: Environment) = env.averagePrice(index, month) + premium
}

case class PartialAveragePricingSpec(index : SingleIndex, dayFractions : Map[Day, Double], premium : Quantity) extends PricingSpec {
  def price(env: Environment) = {
    premium + dayFractions.map{
      case (day, frac) => env.fixingOrForwardPrice(index, day) * frac
    }.sum
  }
}

case class OptionalPricingSpec(choices : List[PricingSpec], declarationDay : Day, chosenSpec : Option[PricingSpec]) extends PricingSpec {
  def price(env: Environment) = {
    assert(chosenSpec.isDefined || env.marketDay < declarationDay.endOfDay, "Optional pricing spec must be fixed by " + declarationDay)
    chosenSpec.getOrElse(choices.head).price(env)
  }
}

case class WeightedPricingSpec(specs : List[(Double, PricingSpec)]) extends PricingSpec{
  def price(env: Environment) = specs.map{case (weight, spec) => spec.price(env) * weight}.sum
}

case class FixedPricingSpec (prices : List[(Quantity, Quantity)]) extends PricingSpec{
  def price(env: Environment) = {
    val totalQuantity = prices.map(_._1).sum
    if (totalQuantity.isZero){
        println("Have no fixed prices")
        Quantity.NULL
    } else {
      prices.map{
        case (qty, prc) => qty * prc
      }.sum / totalQuantity
    }
  }
}

case class PricingSpecConverter(commodityGUID : GUID){
  def indexFromMarket(market : GUID) : SingleIndex = null

  def fromEdmPricingSpec(edmPricingSpec : EDMPricingSpec) : PricingSpec = {
    edmPricingSpec match {
      case spec : EDMMonthAveragePricingSpec => {
        MonthAveragePricingSpec(
          indexFromMarket(spec.market),
          Day.fromJodaDate(spec.qpMonth).containingMonth,
          fromEDMQuantity(spec.premium)
        )
      }
      case spec : EDMPartAvePrcSpec => {
        val dayQuantities = spec.dayQtyMap.map{
          case dayQty => Day.fromJodaDate(dayQty.date) -> fromEDMQuantity(dayQty.quantity)
        }.toMap
        val totalQuantity = dayQuantities.map(_._2).sum
        PartialAveragePricingSpec(
          indexFromMarket(spec.market),
          dayQuantities.map{
            case (day, qty) => day -> (qty / totalQuantity).value
          },
          fromEDMQuantity(spec.premium)
        )

      }
      case spec : EDMOptPricingSpec => {
        OptionalPricingSpec(
          spec.choices.map(fromEdmPricingSpec(_)),
          Day.fromJodaDate(spec.declarationBy),
          if (spec.chosenSpec == null)
            None
          else
            Some(fromEdmPricingSpec(spec.chosenSpec))
        )
      }
      case spec : EDMWtdPricingSpec => {
        WeightedPricingSpec(
          spec.wtdSpecs.map{
            case weightedSpec =>
               (weightedSpec.weight, fromEdmPricingSpec(weightedSpec.pricingSpec))
          }
        )
      }
      case spec : EDMUnkPricingSpec => {
        UnknownPricingSpecification(
           indexFromMarket(spec.market),
           Day.fromJodaDate(spec.qpMonth).containingMonth,
           fromEDMQuantity(spec.quantity),
           spec.fixations.map{
             case fixation =>
              UnknownPricingFixation(fromEDMQuantity(fixation.fixedQuantity), fromEDMQuantity(fixation.observedPrice))
           },
           Day.fromJodaDate(spec.declarationBy),
           fromEDMQuantity(spec.premium)
        )
      }
      case spec : EDMFixedPricingSpec => {
        FixedPricingSpec(
          spec.comps.map{
            case comp =>
              (fromEDMQuantity(comp.quantity), fromEDMQuantity(comp.price))
          }
        )
      }

      case _ => throw new Exception("Unrecognised spec " + edmPricingSpec)
    }
  }
}
case class UnknownPricingFixation(quantity : Quantity, price : Quantity)
case class UnknownPricingSpecification(
  index : SingleIndex,
  month : Month,
  specQuantity : Quantity,
  fixations : List[UnknownPricingFixation],
  declarationDay : Day,
  premium : Quantity
)
  extends PricingSpec
{
  def price(env: Environment) = {
    val totalFixed = fixations.map(_.quantity).sum
    val thirdWednesday = month.firstDay.dayOnOrAfter(DayOfWeek.wednesday) + 14
    val unfixedPriceDay = if (env.marketDay >= thirdWednesday.endOfDay)
      month.lastDay.thisOrPreviousBusinessDay(index.businessCalendar)
    else
      thirdWednesday
    val unfixedQuantity = specQuantity - totalFixed
    val fixedPayment = fixations.map{f => f.quantity * f.price}.sum
    val unfixedPayment = env.fixingOrForwardPrice(index, unfixedPriceDay) * unfixedQuantity
    (unfixedPayment + fixedPayment) / specQuantity
  }
}

object EnvironmentLocator{
  val snapshots = scala.collection.mutable.Map[String, SnapshotID]()
  val lock = new Object()
  def updateSnapshotCache(){
    lock.synchronized {
      Server.server.marketDataStore.snapshots().foreach {
        s: SnapshotID =>
          snapshots += s.id.toString -> s
      }
    }
  }
  def snapshotID(id : String) : SnapshotID = {
    snapshots.getOrElse(id,
      {
        updateSnapshotCache()
        assert(snapshots.contains(id), "Snapshit ID " + id + " not found")
        snapshots(id)
      }
    )
  }
  def snapshotIDsForDay(day : Day) : List[String] = {
    snapshots.filter{
      case (id, snapshotID) => snapshotID.marketDataSelection.pricingGroup == Some(PricingGroup.Metals)
    }.map(_._1).toList
  }
  def environment(id : String) : Environment = {
    val snapshot = snapshotID(id)
    val reader = new NormalMarketDataReader(Server.server.marketDataStore, MarketDataIdentifier(snapshot.marketDataSelection, snapshot.version))
    ClosesEnvironmentRule.createEnv(snapshot.observationDay, reader).environment
  }
}

class QuotaValuer(env : Environment) {
  def commodityFromGUID(guid : GUID) : Commodity = Aluminium
  def getQuota(quotaID : Int) : EDMQuota   = null
  def value(quotaID : Int) : Quantity = value(getQuota(quotaID))
  def value(quota : EDMQuota) : Quantity = {
    val detail = quota.detail
    val commodityGUIDs = detail.deliverySpecs.map(_.materialSpec.asInstanceOf[CommoditySpec].commodity).toSet
    assert(commodityGUIDs.size == 1, "Quota can only have a single commodity, received " + commodityGUIDs)
    if (detail.pricingSpec == null){
      println(quota.quotaNumber  + " has no pricing spec")
      Quantity.NULL
    } else {
      val pricingSpec = PricingSpecConverter(commodityGUIDs.head).fromEdmPricingSpec(detail.pricingSpec)
      pricingSpec.price(env)
    }
  }
}