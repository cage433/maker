package starling.utils

import starling.quantity.UOM
import starling.instrument.Instrument
import starling.daterange.{DayAndTime, Day, Month, DateRangePeriod}
import starling.utils.CollectionUtils._
import starling.curves._
import starling.utils.cache.CacheFactory
import starling.marketdata.MarketData
import starling.instrument.UTP

object AtomicDatumKeyUtils {

  def environmentDifferentiables(inst : Instrument, marketDay : DayAndTime, ccy : UOM) : Set[EnvironmentDifferentiable] = {
    atomicDatumKeys(inst, marketDay, ccy).flatMap(EnvironmentDifferentiable.toEnvironmentDifferentiable)
  }
  def priceKeys(inst : Instrument, marketDay : DayAndTime, ccy : UOM) : Set[EnvironmentDifferentiable with PriceKey] = {
    environmentDifferentiables(inst, marketDay, ccy).filter(_.isInstanceOf[PriceKey]).asInstanceOf[Set[EnvironmentDifferentiable with PriceKey]]
  }
  def volKeys(inst : Instrument, marketDay : DayAndTime, ccy : UOM) : Set[EnvironmentDifferentiable with VolKey] = {
    environmentDifferentiables(inst, marketDay, ccy).filter(_.isInstanceOf[VolKey]).asInstanceOf[Set[EnvironmentDifferentiable with VolKey]]
  }
  def curveKeys(inst : Instrument, marketDay : DayAndTime, ccy : UOM) : Set[CurveKey] = {
    atomicDatumKeys(inst, marketDay, ccy).map(_.curveKey)
  }

  val atomicKeysCache = CacheFactory.getCache("AtomicDatumKeyUtils.atomicKeysCache", unique = true)
  def atomicDatumKeys(trade: Instrument, marketDay: DayAndTime, valuationCCY: UOM): Set[AtomicDatumKey] = {
    val cachingInstrument = trade match {
      case utp : UTP => utp.atomicKeyCachingUTP
      case _ => trade
    }
    atomicKeysCache.memoize(
      (cachingInstrument, marketDay, valuationCCY),
      cachingInstrument.atomicMarketDataKeys(marketDay, valuationCCY)
      )
  }
}
