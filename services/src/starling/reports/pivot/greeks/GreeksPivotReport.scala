package starling.reports.pivot.greeks

import starling.daterange._
import starling.curves._
import starling.market.CommodityMarket
import starling.reports.pivot.PivotReport._
import starling.quantity.UOM._
import starling.quantity.UOM
import starling.pivot.{PivotQuantityFieldDetails, SumPivotQuantityFieldDetails, PivotQuantity}
import starling.pivot.{PivotQuantity => PQ}
import starling.reports.pivot._
import starling.instrument.UTP
import starling.gui.api.{ReportSpecificChoices, ReportSpecificOptions, UTPIdentifier}

@serializable
class GreeksPivotReport(@transient environment : Environment, thetaDayAndTime : DayAndTime, val utps: Map[UTPIdentifier, UTP]) extends PivotReport[GreeksPivotReport.T] {
  def this(environment : Environment, utps : Map[UTPIdentifier, UTP]) = this(environment, environment.marketDay + 1, utps)
  import GreeksPivotReport._

  val ccy = UOM.USD
  val nonSkewEnv = environment.setShiftsCanBeIgnored(true)

  val spreadMonths = PivotReport.spreadMonthsByStrategyAndMarket(utps)
  val swapIndices = PivotReport.swapIndicesByStrategy(utps)

  private def addGreeks(utp : UTP, reportSpecificChoices : ReportSpecificChoices) : List[GreekValues] = {
      val useSkew = reportSpecificChoices.getOrElse(useSkew_str, true)
      val positionOnly = reportSpecificChoices.getOrElse(positionOnly_str, false)
      val collapseOptions = reportSpecificChoices.getOrElse(collapseOptions_str, true)
      val atmVega = reportSpecificChoices.getOrElse(atmVega_str, false)
      val (priceDiffs, volDiffs) = PivotReportUtils.priceAndVolKeys(utp, environment.marketDay, reportSpecificChoices)

      val actualEnv = if (useSkew)
        environment
      else
        nonSkewEnv

      val instrumentPrice = PivotQuantity.calcOrCatch(utp.price(actualEnv))

      val positionGreeks = priceDiffs.toList.map{
        key => 
            new GreekValues(
              null, utp, Some(key), 
              price = PivotQuantity.calcOrCatch(key.quantityValue(actualEnv)),
              position = PivotQuantity.calcOrCatch(utp.position(actualEnv, key))
            )
      }

      var greeks = List[GreekValues]()
      if ( !volDiffs.isEmpty && !positionOnly) {
        greeks :::= priceDiffs.toList.map {
          key => {
              new GreekValues(null, utp, Some(key),
              gamma = PivotQuantity.calcOrCatch(utp.gamma(actualEnv, key, ccy, priceDiffs.toList)),
              deltaBleed = PivotQuantity.calcOrCatch(utp.deltaBleed(actualEnv, key, thetaDayAndTime, ccy)),
              gammaBleed = PivotQuantity.calcOrCatch(utp.gammaBleed(actualEnv, key, thetaDayAndTime, ccy, priceDiffs.toList)))
          }
        }

        greeks :::= volDiffs.toList.map {
          key => {
            new GreekValues(
              null, utp, Some(key), 
              volatility = PivotQuantity.calcOrCatch(
                if (atmVega)
                  key.quantityValue(actualEnv)
                else
                  utp.interpolatedVol(actualEnv, key)
              ),
              vega = PivotQuantity.calcOrCatch(utp.vega(actualEnv, key, shiftInterpolatedVols = !atmVega)),
              vomma = PivotQuantity.calcOrCatch(utp.vomma(actualEnv, key, volDiffs.toList, shiftInterpolatedVols = !atmVega)))
          }
        }
      }
      (positionGreeks ::: greeks).map{
        row =>
          row.copy(
            collapseOptions = collapseOptions,
            instrumentPrice = instrumentPrice,
            period = row.diff.get.periodKey,
            marketName = row.diff.get.riskMarket,
            scale = 1.0
          )
      }
    }

  def rows(utpID: UTPIdentifier, utp: UTP) = List(GreekValues.fromUTP(utpID, utp))

  def scale(risk: GreekValues, volume: Double) = risk * volume

  def fields = GreeksPivotReportHelper.fields(bucketPeriods, useSkew)

  private def convertUOM(rows : List[GreekValues], uomStr : Any, priceUnits : String) : List[GreekValues] = {

    /*
      Convert UOM
    */
    rows.map{
      row =>
      val mkt : Option[CommodityMarket] = row.diff match {
        case Some(pk : PriceKey) => Some(pk.market)
        case Some(sk : SpreadAtmStdDevAtomicDatumKey) => Some(sk.market)
        case _ => None
      }
      mkt match {
        case Some(market) => {
          def convertQuantity(pq : PQ, uom : UOM) : PQ = {
            pq.quantityValue match {
              case Some(q) => {
                val newUOM = q.uom.replace(market.uom, uom)
                PQ(market.convertUOM(q, newUOM))
              }
              case None => pq
            }
          }
          def convertQuantities(uom : UOM) = {
            val rowWithGreeksConverted = row.copy(
              volatility = convertQuantity(row.volatility, uom),
              position = convertQuantity(row.position, uom),
              gamma = convertQuantity(row.gamma, uom),
              vega = convertQuantity(row.vega, uom),
              vomma = convertQuantity(row.vomma, uom),
              deltaBleed = convertQuantity(row.deltaBleed, uom),
              gammaBleed = convertQuantity(row.gammaBleed, uom)
            )
            if (priceUnits == quoted_str) {
              rowWithGreeksConverted
            } else {
              rowWithGreeksConverted.copy(price = convertQuantity(row.price, uom))
            }
          }
          uomStr match {
            case `default_str` => row
            case `barrel_str` => convertQuantities(BBL)
            case `tonne_str` => convertQuantities(MT)
            case `cubic_metre_str` => convertQuantities(M3)
            case `usd_str` => {
              (row.price.quantityValue, row.position.quantityValue) match {
                case (Some(priceQty), Some(volumeQty)) => row.copy(
                  position = PQ(priceQty * volumeQty)
                )
              case _ => row
              }
            }
          }
        }
        case None => row
      }
    }
  }

  override def reportSpecificOptions : ReportSpecificOptions = {
    super.reportSpecificOptions :+ 
      (positionType_str -> List(default_str, barrel_str, cubic_metre_str, tonne_str, usd_str)) :+
      (price_unit_str -> List(position_str, quoted_str)) :+
      (lots_str -> List(false, true)) :+
      (atmVega_str -> List(false, true))
  }

  override def combine(rows: List[GreekValues], reportSpecificChoices: ReportSpecificChoices): List[GreekValues] = {
    import starling.concurrent.MP._
    
    var combinedRows : List[GreekValues] = new PivotUTPRestructurer(environment, reportSpecificChoices, spreadMonths, swapIndices).transformUTPs(rows).mpFlatMap{
      case UTPWithIdentifier(utpID, utp) => {
        val (unitUTP, volume) = utp.asUnitUTP
        addGreeks(unitUTP, reportSpecificChoices).map(_.copy(utpID = utpID)).map(_ * volume)
      }
    }

    combinedRows = convertUOM(combinedRows, reportSpecificChoices.getOrElse(positionType_str, default_str), reportSpecificChoices.getOrElse(price_unit_str, position_str))
    if (reportSpecificChoices.getOrElse(lots_str, false))
      combinedRows = LotConverter.convertGreeks(combinedRows)
    combinedRows = combinedRows.filterNot{
      row => row.position.isAlmostZero && row.gamma.isAlmostZero && row.vega.isAlmostZero
    }
    combinedRows = combinedRows.map(_.setPeriodForDisplay(reportSpecificChoices.getOrElse(tenor_str, Month)))

    combinedRows
  }
}

object GreeksPivotReport extends PivotReportType {
  type T = GreeksPivotReportHelper.T
  private val useSkew = true
  private val bucketPeriods = true

  def name = "Greeks"

  def slidable = true

  def run(context: ReportContext, utps: Map[UTPIdentifier, UTP]) = {
    new GreeksPivotReport(context.environment, context.thetaDayAndTime, utps)
  }

  override def fields = GreeksPivotReportHelper.fields(bucketPeriods, useSkew)

  val useSkew_str = "Skew"
  val positionType_str: String = "Position"
  val position_str: String = "Position"
  val barrel_str: String = "Barrel"
  val tonne_str: String = "Tonne"
  val cubic_metre_str = "<html>m<sup>3</sup></html>"
  val positionOnly_str = "Position Only"
  val default_str: String = "Default"
  val quoted_str = "Quoted"
}

object GreeksPivotReportHelper {
  type T = GreekValues

  // values and labels
  private val values: List[PivotReportField[GreekValues]] = GreekValues.fields.map {
    case (label) =>
      new PivotReportField[GreekValues](label) {
        def value(reportRow: GreekValues) = reportRow.get(label)

        override def pivotFieldDetails =
          if (label == "Market Price")
            new AveragePriceFieldDetails(name)
          else if (label == "Current Price")
            new PivotQuantityFieldDetails(name)
          else if (label == "Volatility")
            new AverageVolatilityFieldDetails(name)
          else
            new SumPivotQuantityFieldDetails(name)
      }
  }

  def fields(bucketPeriods: Boolean, useSkew: Boolean) = values ::: GreeksRiskFields.riskFields
}

object GreeksRiskFields extends RiskPivotFields[GreekValues]
