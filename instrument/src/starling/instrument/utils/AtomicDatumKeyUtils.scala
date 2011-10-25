package starling.instrument.utils

import starling.quantity.UOM
import starling.instrument.Instrument
import starling.daterange.{DayAndTime, Day, Month, DateRangePeriod}
import starling.utils.CollectionUtils._
import starling.curves._
import starling.utils.cache.CacheFactory
import starling.marketdata.MarketData
import starling.instrument.UTP
import starling.marketdata.ReferenceDataLookup

object AtomicDatumKeyUtils {

  def environmentDifferentiables(inst : Instrument, env : Environment, ccy : UOM) : Set[EnvironmentDifferentiable] = {
    atomicDatumKeys(inst, env, ccy).flatMap(EnvironmentDifferentiable.toEnvironmentDifferentiable)
  }

  def priceKeys(inst : Instrument, env : Environment, ccy : UOM) : Set[EnvironmentDifferentiable with PriceKey] = {
    environmentDifferentiables(inst, env, ccy).filter(_.isInstanceOf[PriceKey]).asInstanceOf[Set[EnvironmentDifferentiable with PriceKey]]
  }

  def volKeys(inst : Instrument, env : Environment, ccy : UOM) : Set[EnvironmentDifferentiable with VolKey] = {
    environmentDifferentiables(inst, env, ccy).filter(_.isInstanceOf[VolKey]).asInstanceOf[Set[EnvironmentDifferentiable with VolKey]]
  }

  def curveKeys(inst : Instrument, env : Environment, ccy : UOM) : Set[CurveKey] = {
    atomicDatumKeys(inst, env, ccy).map(_.curveKey)
  }

  val atomicKeysCache = CacheFactory.getCache("AtomicDatumKeyUtils.atomicKeysCache", unique = true)
  def atomicDatumKeys(trade: Instrument, env : Environment, valuationCCY: UOM): Set[AtomicDatumKey] = {
    val cachingInstrument = trade match {
      case utp : UTP => utp.atomicKeyCachingUTP
      case _ => trade
    }
    atomicKeysCache.memoize(
      (cachingInstrument, env.marketDay, valuationCCY),
      cachingInstrument.atomicMarketDataKeys(env, valuationCCY)
      )
  }
}
