package starling.instrument

import starling.curves.Environment
import starling.curves.EnvironmentDifferentiable
import starling.quantity.UOM._
import starling.quantity.Quantity
import starling.quantity.Quantity._
import starling.curves.ForwardPriceKey
import starling.curves.FuturesSpreadPrice
import starling.curves.PriceDifferentiable
import starling.curves.SwapPrice
import starling.daterange._
import starling.utils.{CollectionUtils, SummingMap}
import starling.utils.SummingMap
import starling.daterange._
import starling.market.{Index, FuturesFrontPeriodIndex, FuturesMarket}

object Hedge{
  private def calcHedge(
    env : Environment, 
    diffs : List[EnvironmentDifferentiable], 
    instrument : Instrument, 
    newDiffs : List[EnvironmentDifferentiable]
  ) : Map[EnvironmentDifferentiable, Quantity] = {

    val derivs = diffs.map{diff => (diff, instrument.firstOrderDerivative(env, diff, USD)) }.toMap
    EnvironmentDifferentiable.transform(env, derivs, newDiffs)
  }

  private def asListOfUTPS(map : Map[UTP, Double]) : List[UTP] = {
    map.map{
      case (hi, x) => hi * x
    }.toList
  }

  def futuresSpreadHedge(env : Environment, liveFutures : List[Future], spreadMonths : Set[Month]) : Map[UTP, Double] = {

    if (liveFutures.isEmpty)
      return Map()
    val market = liveFutures.head.market
    assert(liveFutures.forall(_.market == market), "All liveFutures must be for the same market")
    assert(market.tenor == Month, "Only works for monthly tenors")
    assert(liveFutures.forall{fut => spreadMonths.contains(fut.delivery.asInstanceOf[Month])}, "Not all futures contained in CSO Months")


    val months = spreadMonths.toList.sortWith(_<_)
    val diffs = months.map(PriceDifferentiable(market, _))

    val newDiffs = PriceDifferentiable(market, months.last) :: months.zip(months.tail).map{case (m1, m2) => FuturesSpreadPrice(market, m1 / m2)}

    val strike = Quantity(0, market.priceUOM)
    val spreads = calcHedge(env, diffs, CompositeInstrument(liveFutures), newDiffs).map{
      case (PriceDifferentiable(_, month), volume) => (Future(market, month, strike, Quantity(1, market.uom)).asInstanceOf[UTP], volume.value)
      case (FuturesSpreadPrice(_, SpreadPeriod(m1: Month, m2: Month)), volume) => (FuturesCalendarSpread(market, m1, m2, strike, strike, Quantity(1, market.uom)).asInstanceOf[UTP], volume.value)
    }
    spreads + compensatingBankAccount(env, liveFutures, asListOfUTPS(spreads))
  }

  def futuresSpreadHedgeUTPs(env : Environment, futures : List[Future], spreadMonths : Set[Month]) : List[UTP] = {
    val liveOrDead = futures.groupBy(_.isLive(env.marketDay))
    val spreadHedgeUTPs = asListOfUTPS(futuresSpreadHedge(env, liveOrDead.getOrElse(true, Nil), spreadMonths)) ::: liveOrDead.getOrElse(false, Nil)

    def assertFuturesAndSpreadsHaveTheSameDeltas() {
      val futuresComp = CompositeInstrument(futures)
      val spreadComp = CompositeInstrument(spreadHedgeUTPs)
      for (
        key <- CollectionUtils.filterOnType[PriceDifferentiable](futuresComp.environmentDifferentiables(env.marketDay) ++ spreadComp.environmentDifferentiables(env.marketDay))
      ) {
        val delta = futuresComp.firstOrderDerivative(env, key, USD)
        val hedgeDelta = spreadComp.firstOrderDerivative(env, key, USD)
        assert((delta - hedgeDelta).abs.value < 1e-5, "Converting futures to spreads has failed to make a portfolio with the same delta")
      }
    }
    assertFuturesAndSpreadsHaveTheSameDeltas()
    spreadHedgeUTPs
  }

  def hedgeBankAccountsLikeSpreads(acc : List[BankAccount]) : List[BankAccount] = {
    if (acc.isEmpty)
      return Nil
    val market = acc.head.market.get
    assert(acc.forall(_.market.get == market))
    val monthlyNetVolumes = acc.groupBy{
      case BankAccount(_, Some(mkt : FuturesMarket), _, DateRangePeriod(m : Month)) => m
      case o => throw new Exception("Unexpected bank account " + o)
    }.mapValues(_.map(_.volume).sum)
    val months = monthlyNetVolumes.keySet.toList.sortWith(_<_)
    BankAccount(monthlyNetVolumes(months.last), Some(market), None, DateRangePeriod(months.last)) ::
      months.dropRight(1).map{mth => BankAccount(monthlyNetVolumes(mth), Some(market), None, SpreadPeriod(mth, mth + 1))}
  }

  private def netFuturesByContiguousMonths(marketDay : DayAndTime, futures : List[Future]) : List[List[Future]]= {
    val market = futures.head.market
    val contiguousMonthGroups = Month.contiguousMonths(futures.map(_.delivery.asInstanceOf[Month]).toSet.toList.sortWith(_<_))

    var netPositions = SummingMap[Month]()
    var netStrikeValues = SummingMap[Month]()
    for (
      future <- futures;
      if future.isLive(marketDay)
    ) {
      netPositions += (future.delivery.asInstanceOf[Month], future.volume)
      netStrikeValues += (future.delivery.asInstanceOf[Month], future.volume * future.strike)
    }

    val byMonthGroup = netPositions.underlying.groupBy{
      case (mth, _) => contiguousMonthGroups.find(_.contains(mth)).get
    }

    byMonthGroup.map{
      case (months, positions) => {
        val futures = positions.map{
          case (month, volume) => {
            val strike = if (volume.isZero) Quantity(0, market.priceUOM) else netStrikeValues(month) / volume
            Future(market, month, strike, volume)
          }
        }.toList.sortWith(_.delivery < _.delivery)
        futures
      }
    }.toList
  }

  private def breakContiguousFuturesBySign(futures : List[Future]) : List[List[Future]] = {
    def recurse(futuresLeft : List[Future], currentGroup : List[Future], acc : List[List[Future]]) : List[List[Future]] = {
      (futuresLeft, currentGroup) match {
        case (Nil, _) => (currentGroup :: acc).map(_.reverse).filterNot(_.isEmpty)
        case (fut :: rest, Nil) => recurse(rest, List(fut), acc)
        case (fut :: rest, fut2 :: rest2) if fut.volume.sign == fut2.volume.sign => recurse(rest, fut :: fut2 :: rest2, acc)
        case (fut :: rest, group) => recurse(rest, List(fut), group :: acc)
      }
    }
    recurse(futures, Nil, Nil)
  }

  private def swapHedge(env : Environment, liveFutures : List[Future]) : List[Map[UTP, Double]] = {
    if (liveFutures.isEmpty)
      return Nil

    val market = liveFutures.head.market
    assert(liveFutures.forall(_.market == market), "All liveFutures must be for the same market")

    if (!Index.futuresMarketToIndexMap.contains(market))
      return List(liveFutures.map(_ -> 1.0).toMap)
    if (market.tenor != Month)
      return List(liveFutures.map(_ -> 1.0).toMap)

    val index = Index.futuresMarketToIndexMap(market)
    netFuturesByContiguousMonths(env.marketDay, liveFutures).flatMap(breakContiguousFuturesBySign).map{
      case contiguousFutures => {
        val months = contiguousFutures.map(_.delivery.asInstanceOf[Month])
        val diffs = months.map(PriceDifferentiable(market, _))
        val newDiffs = PriceDifferentiable(market, months.last) ::
            months.dropRight(1).map{m => SwapPrice(index, index.frontFuturesMonthToSwapMonth(m))}
        val strike = Quantity(0, market.priceUOM)

        val hedges : Map[UTP, Double] = calcHedge(env, diffs, CompositeInstrument(contiguousFutures), newDiffs).map(
          _ match {
            case (PriceDifferentiable(_, month), volume) => (Future(market, month, strike, Quantity(1, market.uom)), volume.value)
            case k @ (SwapPrice(_, m), volume) => {
              val swap = SinglePeriodSwap(index, strike, Quantity(1, market.uom), m, cleared = true)
              val swapDelta = swap.firstOrderDerivative(env, k._1, swap.valuationCCY)
              val swapUndiscountedDelta = swap.firstOrderDerivative(env.undiscounted, k._1, swap.valuationCCY)
              val swapPosition = swap.position(env, k._1)
              val swapDiscountedPosition = swapPosition * swapDelta / swapUndiscountedDelta
              (swap, volume.checkedValue(index.uom) / swapDiscountedPosition.checkedValue(index.uom))
          }

          }
        )
        hedges + compensatingBankAccount(env, contiguousFutures, asListOfUTPS(hedges))
      }
    }.toList
  }

  def hedgeBankAccountsLikeSwaps(index : FuturesFrontPeriodIndex, acc : List[BankAccount]) : List[BankAccount] = {
    if (acc.isEmpty)
      return acc
    var monthlyNetVolumes = SummingMap[Month]()
    acc.foreach{
      case BankAccount(volume, None, Some(_), DateRangePeriod(dr : DateRange)) if dr.firstMonth == dr.lastMonth => monthlyNetVolumes += (dr.firstMonth -> volume)
      case other => throw new Exception("Unexpected bank account " + other)
    }
    val months = monthlyNetVolumes.keySet.toList.sortWith(_<_)
    BankAccount(monthlyNetVolumes(months.last), Some(index.market), None, DateRangePeriod(months.last)) ::
      months.dropRight(1).map{mth => BankAccount(monthlyNetVolumes(mth), None, Some(index), DateRangePeriod(index.frontFuturesMonthToSwapMonth(mth)))}
  }

  private def compensatingBankAccount(env : Environment, futures : List[Future], hedgeUTPs : List[UTP]) : (BankAccount, Double) = {
    val futuresMTM = CompositeInstrument(futures).mtm(env)
    val month = futures.map(_.delivery).sortWith(_<_).last
    val hedgeMTM = CompositeInstrument(hedgeUTPs).mtm(env)
    val difference = futuresMTM - hedgeMTM
    val bankAccount = BankAccount(1.0(futuresMTM.uom), Some(futures.head.market), None, month)
    (bankAccount, difference.value)
  }
  def swapHedgeUTPs(env : Environment, futures : List[Future]) : List[UTP] = {
    val liveOrDead = futures.groupBy(_.isLive(env.marketDay))
    swapHedge(env, liveOrDead.getOrElse(true, Nil)).flatMap(asListOfUTPS(_)) ::: liveOrDead.getOrElse(false, Nil)
  }
}
