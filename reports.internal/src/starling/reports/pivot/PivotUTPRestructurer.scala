package starling.reports.pivot

import starling.market.FuturesFrontPeriodIndex
import starling.utils.cache.CacheFactory
import starling.pivot.PivotQuantity
import starling.reports.pivot.PivotReport._
import starling.quantity.{UOM, Quantity}
import starling.curves._
import starling.utils.{CollectionUtils}
import starling.instrument._
import starling.market.FuturesMarket
import starling.daterange._
import starling.gui.api.{ReportSpecificChoices, UTPIdentifier}

/**
 * The pivot reports transform the UTPs (via collapseOptions, equivalentFutures and recombining
 * futures into spreads and/or swaps. Reporting is then done on the riskInstrument rather
 * than the UTP
 */
case class UTPWithIdentifier(
  id : UTPIdentifier,
  utp : UTP
){
  def strategy = id.getAttribute(strategy_str)
}

class PivotUTPRestructurer(
  environment : Environment, 
  reportSpecificChoices : ReportSpecificChoices,
  spreadMonthsByStrategyAndMarket : Map[(Option[String], FuturesMarket), Set[Month]],
  indicesByStrategy : Map[Option[String], Set[FuturesFrontPeriodIndex]]
){
  /**
    Given a list of UTPs, return an equivalent list (in terms of position and MTM) such that
    futures are replaced by futures spreads. A similar transformation is applied to bank accounts 
    (these will have come from the cash part of a Future's UTP portfolio. The reason for the latter
    is to prevent a proliferation of risk periods in the advanced OLAP report
  **/
  def convertFuturesToFuturesSpreads(utps : List[UTPWithIdentifier]) : List[UTPWithIdentifier] = {

    def convertICEToNymex(utp : UTP) : UTP = {
       // Hack for JF. When 'Futures as Spreads' is chosen he has no
       // wish to separate ICE and NYMEX positions. Hence we simply
       // convert the UTPs.
      import starling.market.Market._
      utp match {
        case fut : Future if fut.market == ICE_WTI => fut.copy(market = NYMEX_WTI)
        case opt : SingleCalendarSpreadOption if opt.market == ICE_WTI => opt.copy(market = NYMEX_WTI)
        case ba : BankAccount if ba.market == Some(ICE_WTI) => ba.copy(market = Some(NYMEX_WTI))
        case other => other
      }
    }
    val utpsAsNymex = utps.map{case UTPWithIdentifier(id, utp) => UTPWithIdentifier(id, convertICEToNymex(utp))}
    utpsAsNymex.groupBy(_.strategy).flatMap{
      case (strategyID, strategyUTPs) =>
      val byInstrumentUTPs = strategyUTPs.map(_.utp).groupBy(_.instrumentType)
      val futuresByMarket = byInstrumentUTPs.getOrElse(Future, Nil).map(_.asInstanceOf[Future]).groupBy(
        _.market
      )
      val bankAccountsByMarket = byInstrumentUTPs.getOrElse(BankAccount, Nil).map(_.asInstanceOf[BankAccount]).groupBy(
        _.market
      )

      val transformedUTPs = futuresByMarket.flatMap{
        case (mkt, futures) =>
          spreadMonthsByStrategyAndMarket.get((strategyID, mkt)) match {
            case Some(spreadMonths) => {
              val (transformableFutures, nonTransformableFutures) = futures.partition{
                case Future(_, month : Month, _, _) if spreadMonths.contains(month) => true
                case _ => false
              }
              val transformedFutures = Hedge.futuresSpreadHedgeUTPs(environment, transformableFutures, spreadMonths) ::: nonTransformableFutures

              val (transformableBankAccounts, nonTransformableBankAccounts) = bankAccountsByMarket.getOrElse(Some(mkt), Nil).partition{
                case BankAccount(_, _, _, DateRangePeriod(month : Month)) if spreadMonths.contains(month) => true
                case _ => false
              }
              val transformedBankAccounts = Hedge.hedgeBankAccountsLikeSpreads(transformableBankAccounts) ::: nonTransformableBankAccounts
              transformedFutures ::: transformedBankAccounts
            }
            case None => futures ::: bankAccountsByMarket.getOrElse(Some(mkt), Nil)
          }

      }.toList

      val nonTransformedBankAccounts = List.concat(
        bankAccountsByMarket.filterKeys{ _ match {
          case Some(mkt) => !futuresByMarket.keySet.contains(mkt)
          case None => true
        }}.valuesIterator.toList : _*
      )
      val allUTPs : List[UTP] = transformedUTPs ::: nonTransformedBankAccounts :::
        List.concat(byInstrumentUTPs.filterKeys{ _ match
        {
          case Future => false
          case BankAccount => false
          case _ => true
        }}.valuesIterator.toList : _*)

      allUTPs.map{
        utp =>
          UTPWithIdentifier(
            UTPIdentifier(0, strategyID.map{id => Map("strategyID" -> id)}.getOrElse(Map())),
            utp
          )
      }
    }.toList
  }

  private def convertFuturesToSwaps(rows : List[UTPWithIdentifier]) : List[UTPWithIdentifier] = {
    rows.groupBy(_.strategy).flatMap{
      case (strategyID, strategyRows) => {
        val byInstrumentUTPs = strategyRows.map(_.utp).groupBy(_.instrumentType)
        if (! indicesByStrategy.contains(strategyID))
          strategyRows
        else {
          val indices = indicesByStrategy(strategyID)
          val frontlineMarkets = indices.map(_.market)
          val futuresByMarket = byInstrumentUTPs.getOrElse(Future, Nil).map(_.asInstanceOf[Future]).groupBy(
            _.market
          )
          val bankAccountsByIndex = byInstrumentUTPs.getOrElse(BankAccount, Nil).map(_.asInstanceOf[BankAccount]).groupBy(
            _.index match {
              case Some(idx : FuturesFrontPeriodIndex) => Some(idx)
              case _ => None
            }
          )
          val transformedFutures = indices.toList.flatMap{
            idx =>
              Hedge.swapHedgeUTPs(environment, futuresByMarket.getOrElse(idx.market, Nil))
          }
          val transformedBankAccounts = indices.toList.flatMap{
            idx =>
              Hedge.hedgeBankAccountsLikeSwaps(idx, bankAccountsByIndex.getOrElse(Some(idx), Nil))
          }
          val nonTransformedFutures = List.concat(
            futuresByMarket.filterKeys(! frontlineMarkets.contains(_)).valuesIterator.toList : _*
          )
          val nonTransformedBankAccounts = List.concat(
            bankAccountsByIndex.filterKeys{ _ match {
              case Some(idx) => !indices.contains(idx)
              case None => true
            }
          }.valuesIterator.toList : _*)
          val allUTPs : List[UTP] = transformedFutures ::: nonTransformedFutures :::
            transformedBankAccounts ::: nonTransformedBankAccounts :::
            List.concat(byInstrumentUTPs.filterKeys{ _ match
            {
              case Future => false
              case BankAccount => false
              case _ => true
            }}.valuesIterator.toList : _*)
          allUTPs.map{
            utp =>
              UTPWithIdentifier(
                UTPIdentifier(0, strategyID.map{id => Map("strategyID" -> id)}.getOrElse(Map())),
                utp
              )
          }
        }
      }
    }.toList
  }

  def apply(utps : List[UTPWithIdentifier]) : List[UTPWithIdentifier] = {
    val futuresAsSpreads = reportSpecificChoices.getOrElse(futuresAsSpreads_str, false)
    val futuresAsSwaps = reportSpecificChoices.getOrElse(futuresAsSwaps_str, false)
    val useSkew = reportSpecificChoices.getOrElse(useSkew_str, true)

    var result = utps

    if (futuresAsSpreads){
      result = try {
        convertFuturesToFuturesSpreads(result)
      } catch {
        case t:Throwable => result
      }
    }
    if (futuresAsSwaps){
      result = try {
        convertFuturesToSwaps(result)
      } catch {
        case t:Throwable => result
      }
    }
    result
  }

  def transformUTPs(rows : List[RiskPivotReportRow[_]]) = {
    var utpsWithIdentifier = rows.map{
      row => UTPWithIdentifier(row.utpID, row.utp * row.scale)
    }.toList
    apply(utpsWithIdentifier)
  }
}

object PivotReportUtils{

  def priceAndVolKeys(utp : UTP, marketDay : DayAndTime, reportSpecificChoices : ReportSpecificChoices)
    : (
      Set[EnvironmentDifferentiable with PriceKey], Set[EnvironmentDifferentiable with VolKey]
      ) = {
      UTP.priceAndVolKeys(utp, marketDay, reportSpecificChoices.getOrElse(showEqFutures_str, true), reportSpecificChoices.getOrElse(tenor_str, Month))
  }


}


