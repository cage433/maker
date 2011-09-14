package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.db._
import collection.immutable.TreeMap
import starling.marketdata._
import starling.quantity.{Conversions, UOM, Quantity, Percentage}
import starling.market._
import starling.gui.api._
import starling.rmi.StarlingServerImpl
import starling.services.PricingGroupIDUtil
import starling.auth.User
import starling.loopyxl.ExcelMethod
import starling.services.trade.instrumentreaders.ExcelInstrumentReader
import starling.curves.{USDFXRateCurveKey, SpreadStdDevSurfaceData, SpreadStdDevSurfaceDataKey}
import starling.utils.{Log, Broadcaster}
import starling.pivot.MarketValue
import org.boris.xlloop.reflect.XLFunction._
import starling.daterange._

class MarketDataHandler(broadcaster : Broadcaster,
                   starlingServer : StarlingServerImpl,
                   marketDataStore: MarketDataStore) {

  @ExcelMethod
  @XLFunction(
    name = "observationPoint",
    category = "Starling")
  def observationPoint(day:Double, time:String) = {
    ObservationPoint(Day.fromExcel(day), ObservationTimeOfDay.fromName(time)).unparse
  }

  @ExcelMethod
  @XLFunction(
    name = "UPLOADVOLS",
    category = "Starling",
    args = Array("identifier", "market", "dates", "at the money", "deltas"),
    argHelp = Array(
      "String identifier for the set of curves to upload.",
      "The name of the market",
      "Range of dates, each of which must line up with some data.",
      "At-the-money vols for each date - the ranges must be the same size.",
      "Range with the header row as a set of deltas from the ATM strike."))
  def uploadVols(excel: String,
                 observationDate: Object,
                 _marketStr: String,
                 doubleDates: Array[Double],
                 deltas: Array[Array[Object]]): String = {

    val marketStr = _marketStr.trim
    val observationPoint = ObservationPoint.parse(observationDate)

    // TODO [07 Jan 2011] remove hard-coded months - needs to get base tenor from "market"
    val dates = doubleDates.map((d: Double) => Day.fromExcel(d))
    val months = dates.map(_.containingMonth)

    val volData = if (deltas.nonEmpty) {
      val header = deltas.head
      val rows = deltas.tail
      val vols = Map() ++ header.zipWithIndex.map{
        case (v, index) => {
          v -> rows.map(r => Percentage(r(index).asInstanceOf[Double]))
        }
      }
      // if all the ATM vols are over 100% we should scale then by diving by 100
      val multiplier = if(vols("ATM").forall(p => p.isAlmostZero || p.value > 1.0)) {
        0.01
      } else {
        1.0
      }
      val atm = vols("ATM").map(_ * multiplier)
      val strikes = header.filterNot(_ == "ATM").map{
        v => v.asInstanceOf[Double]
      }
      val skews = strikes.map {
        delta => vols(delta.asInstanceOf[java.lang.Double]).map(_ * multiplier)
      }
      OilVolSurfaceData(
        months.toArray,
        atm,
        strikes,
        skews
      )
    } else {
      OilVolSurfaceData(Array(), Array(), Array(), Array())
    }
    val market = ExcelInstrumentReader.commodityMarketOption(marketStr) match {
      case Some(m) => m
      case None => Market.fromName(marketStr)
    }
    val volKey = OilVolSurfaceDataKey(market)

    val version = marketDataStore.save(Map(MarketDataSet.excel(excel) -> List(MarketDataEntry(observationPoint, volKey, volData))))

    val header = deltas.head.map(header => header match {
      case "ATM" => 0.0
      case value => value.asInstanceOf[Double]
    })

    val vols: Array[Array[Double]] = Array(header) ++ deltas.tail.map(_.map(_.asInstanceOf[Double]))

    broadcaster.broadcast(UploadVolsUpdate(User.currentlyLoggedOn, excel, observationPoint.day, dates, marketStr, vols))

    "OK:" + version
  }



  @ExcelMethod
  @XLFunction(
    name = "uploadFXHistory",
    category = "Starling",
    args = Array("label", "priceHistory"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "A range containg the fx history with Currency at the top and days on the left"))
  def uploadFXHistory(label: String, priceHistory: Array[Array[Object]]) = {
    if (label == null) throw new Exception("Label is null")
    assert(label.nonEmpty, "Can't have an empty label for the market data")
    val ccyHeader = Map() ++ priceHistory(0).toList.tail.zipWithIndex.flatMap{
      case (ccy, index) => {
        ccy match {
          case "" => None
          case s: String => Some((index + 1) -> UOM.fromIdentifier(s.replaceAll("=", "").trim)) //+1 beacuse Day is column 0
          case _ => None
        }
      }
    }
    val processedRates = priceHistory.toList.tail.flatMap{
      row => {
        row.head match {
          case d: java.lang.Double => {
            val observationPoint = ObservationPoint.fromExcel(d.doubleValue)
            ccyHeader.flatMap{
              case (index, ccy) => {
                row(index) match {
                  case d: java.lang.Double => {
                    val value = d.doubleValue
                    val rateInUSD = ccy match {
                      case UOM.GBP | UOM.AUD => true; case _ => false
                    }
                    val rate = if (rateInUSD) {
                      Quantity(value, UOM.USD / ccy)
                    } else {
                      Quantity(value, ccy / UOM.USD).invert
                    }
                    Some((observationPoint, ccy, rate))
                  }
                  case _ => None
                }
              }
            }
          }
          case _ => None
        }
      }
    }
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(Map(marketDataSet -> processedRates.map{
      case (observationDay, ccy, spotFX) => {
        MarketDataEntry(observationDay, SpotFXDataKey(ccy), SpotFXData(spotFX))
      }
    }))
    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(category = "Starling", name = "bulkUploadPrices",
    args=Array("label", "observationDate", "data"),
    argHelp = Array(
      "The name for this data (will be shown in the starling dropdown menu)",
      "The observation day to store this data against",
      "A range containing market name at the top and date range on the left. Can include blank columns"
    )
  )
  def bulkUploadPrices(label: String, observationDate: Object, data: Array[Array[Object]]) = {
    assert(label.nonEmpty, "Can't have an empty label for the market data")
    val observationPoint = ObservationPoint.parse(observationDate)
    //look at header to decide on behaviour

    val header = data.head.map {
      case null => ""
      case a => a
    }

    val allPrices: Map[MarketDataKey, PriceData] = {
      if (header.toList.map(_.toString.toLowerCase) == List("market", "period", "price")) {
        val pricesByMarket = data.tail.flatMap { row =>
          val marketName = row(0).asInstanceOf[String].trim
          if (!marketName.isEmpty) {
            val market = Market.fromName(marketName)
            val period = objectToDateRange(observationPoint, row(1), market.tenor).getOrElse(throw new Exception("Can't parse " + row(1)))
            val price = row(2).asInstanceOf[Double]
            (market, period, price) :: Nil
          } else {
            Nil
          }
        }.groupBy(_._1)
        pricesByMarket.map { case (market, prices) => {
          val ppp = prices.map { case(_,period,price) => period->price }
          (PriceDataKey(market), PriceData.create(ppp, market.priceUOM))
        }}.toMap
      } else {
        val markets = header
        val prices = data.tail
        (for ((marketName, index) <- markets.zipWithIndex
              if marketName != null && marketName.isInstanceOf[String] && !marketName.toString.trim().isEmpty) yield {
          val marketStr = marketName.asInstanceOf[String].trim
          val market = ExcelInstrumentReader.commodityMarketOption(marketStr) match {
            case Some(m) => m
            case None => Market.fromName(marketName.asInstanceOf[String])
          }
          val priceRows: Array[(DateRange, Double)] = prices.flatMap{
            row => extractPeriodPricePair(observationPoint, market.tenor, row, index)
          }
          (PriceDataKey(market), PriceData.create(priceRows, market.priceUOM))
        }).toMap
      }
    }

    val result = marketDataStore.saveAll(MarketDataSet.excel(label), observationPoint, allPrices)
    "OK:" + result.maxVersion
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadFX",
    category = "Starling")
  def uploadFX(label:String, observationDate: Object, currencyName: String, rate:Double) = {
    assert(label.nonEmpty, "Can't have an empty label for the market data")
    val observationPoint = ObservationPoint.parse(observationDate)
    val currency = UOM.parseCurrency(currencyName).getOrElse(throw new Exception("Unknown currency " + currencyName))
    val fxRate = new SpotFXData(Quantity(rate, UOM.USD/ currency))
    val result = marketDataStore.save(MarketDataSet.excel(label), TimedMarketDataKey(observationPoint, SpotFXDataKey(currency)), fxRate)
    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadPrices",
    category = "Starling",
    args = Array("label", "prices", "optionalPrices"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "A range containg the prices with period in the first column and price in the second",
      "The prices matching the periods in the previous argument if it was a 1 column range"))
  def uploadPrices(label: String, observationDate: Object, marketName: String, requiredPrices: Array[Array[Object]],
                   optionalPrices: Array[Object]) = {

    assert(label.nonEmpty, "Can't have an empty label for the market data")
    val observationPoint = ObservationPoint.parse(observationDate)
    val market = Market.fromName(marketName.trim)
    val joinedPriceRange: Array[Array[Object]] = if (optionalPrices == null || optionalPrices.size == 0) {
      requiredPrices
    } else {
      requiredPrices.zipWithIndex.map{
        case (row, index) => Array(row(0), optionalPrices(index))
      }
    }
    val priceRows: Array[(DateRange, Double)] = joinedPriceRange.flatMap{
      row => extractPeriodPricePair(observationPoint, market.tenor, row, 1)
    }
    val result = marketDataStore.save(MarketDataSet.excel(label), TimedMarketDataKey(observationPoint, PriceDataKey(market)),
      PriceData.create(priceRows, market.priceUOM))

    val dates: Array[Day] = priceRows.map(_._1.firstDay)
    val prices : Array[Double] = priceRows.map(_._2)

    broadcaster.broadcast(UploadPricesUpdate(User.currentlyLoggedOn, label, observationPoint, dates, marketName, prices))

    "OK:" + result
  }

  private def extractPeriodPricePair(observationPoint: ObservationPoint, tenor:TenorType, row:Array[Object], priceIndex:Int):Option[(DateRange,Double)] = {
    val period = try {
      if (row(0) == null) None else objectToDateRange(observationPoint, row(0), tenor)
    } catch {
      case e: NoSuchElementException => None
      case e: IllegalStateException => None
    }

    val price = row(priceIndex) match {
      case d: java.lang.Double if (d.doubleValue > 0) => Some(d.doubleValue)
      case _ => None
    }

    (period, price) match {
      case (Some(pe), Some(pr)) => Some(pe -> pr)
      case _ => None
    }
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadInterestRates",
    category = "Starling",
    args = Array("label", "observationDay", "currency", "periods", "rates"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "The observation day for this data",
      "The currency for the interest rates",
      "The settlement days for the interest rates",
      "The interest rates (matching the settlement days)"))
  def uploadInterestRates(label: String, observationDate: Object, currency: String, periods: Array[Double], rates: Array[Double]) = {
    assert(label.nonEmpty, "Can't have an empty label for the market data")

    val observationPoint = ObservationPoint.parse(observationDate)
    val ccy = UOM.parseCurrency(currency).getOrElse(throw new Exception(currency + " is not a known currency"))
    val rows = (periods zip rates).flatMap{
      case (period, rate) => {
        if (period != 0) {
          val day = Day.fromExcel(period)
          val entry = ForwardRateDataEntry(day, "Annual", "SWAP", rate)
          List(entry)
        } else {
          Nil
        }
      }
    }
    val result = marketDataStore.save(MarketDataSet.excel(label), TimedMarketDataKey(observationPoint, ForwardRateDataKey(ccy)), ForwardRateData(rows.toList))

    val dates = periods.map((d: Double) => Day.fromExcel(d))
    broadcaster.broadcast(UploadInterestRatesUpdate(User.currentlyLoggedOn, label, observationPoint.day, dates, currency, rates))

    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(
    name = "updateInterestRateFixings",
    args = Array("label", "observationPoint", "interestRateType", "currency", "maturities", "level", "rates"))
  def updateInterestRateFixings(label: String, observationDate: Object, irType: String, currency: String,
                                periods0: Array[Object], levels0: Array[String], rates: Array[Double]) = {
    assert(label.nonEmpty, "Can't have an empty label for the market data")

    val observationPoint = ObservationPoint.parse(observationDate)
    val ccy = UOM.parseCurrency(currency).getOrElse(throw new Exception(currency + " is not a known currency"))
    val periods = periods0.map(StoredFixingPeriod.parse)
    val levels = levels0.map(Level.fromName)
    val percents = rates.map(MarketValue.percentage)

    val result = marketDataStore.save(MarketDataSet.excel(label),
      TimedMarketDataKey(observationPoint, PriceFixingsHistoryDataKey(currency, Some(irType))),
      PriceFixingsHistoryData.create((levels zip periods zip percents)))

    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadStandardDeviations",
    category = "Starling",
    args = Array("label", "observationDay", "marketName", "periods", "standardDeviations"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "The observation day for this data",
      "The market for the standard deviations",
      "The periods (1 column)",
      "The standard deviations with the header 'ATM, CALL, PUT' and rows matching the periods"))
  def uploadStandardDeviations(label: String, observationDate: Object, marketName: String, _periods: Array[Any],
                               _standardDeviations: Array[Array[Object]]) = {
    assert(label.nonEmpty, "Can't have an empty label for the market data")

    // remove empty rows
    var missingRows = Set[Int]()
    val periods = _periods.zipWithIndex.flatMap{
      case (null, i) => missingRows += i; None
      case (a, _) => Some(a.toString)
    }
    val headers = _standardDeviations.head.map(_.toString.toLowerCase)
    val standardDeviations = _standardDeviations.tail.zipWithIndex.filterNot{case (_, i) => missingRows.contains(i)}.map(_._1)

    val observationPoint = ObservationPoint.parse(observationDate)
    val (market, spreadMarket) = ExcelInstrumentReader.marketOption(marketName.trim) match {
      case Some(m: FuturesSpreadMarket) => (m, true)
      case Some(m: FuturesMarket) => (m, false)
      case None => throw new Exception("Invalid market: " + marketName)
    }

    val parsedPeriods: Array[Period] = periods.map {
      case Day(d) if !spreadMarket => {
        val month = d.containingMonth
        new SpreadPeriod(month, month.next)
      }
      case Day(d) if spreadMarket => {
        DateRangePeriod(d.containingMonth)
      }
      case SpreadParse(s) => {
        s: SpreadPeriod
      }
      case "null" => throw new Exception("Range includes empty cells")
    }
    val atmIndex = headers.indexOf("atm")
    val callIndex = headers.indexOf("call")
    val putIndex = headers.indexOf("put")

    val atm = standardDeviations.map{
      row => row(atmIndex).asInstanceOf[Double]
    }
    val call = standardDeviations.map{
      row => row(callIndex).asInstanceOf[Double]
    }
    val put = standardDeviations.map{
      row => row(putIndex).asInstanceOf[Double]
    }

    val key = SpreadStdDevSurfaceDataKey(market)
    val data = SpreadStdDevSurfaceData(parsedPeriods, atm, call, put, market.priceUOM)
    val result = marketDataStore.save(MarketDataSet.excel(label), TimedMarketDataKey(observationPoint, key), data)

    val sds: Array[Array[Double]] = Array(Array(0.5, 0, 1)) ++ standardDeviations.map(_.map(_.asInstanceOf[Double]))

    broadcaster.broadcast(UploadStandardDeviationsUpdate(User.currentlyLoggedOn, label, observationPoint.day, parsedPeriods, marketName, sds))

    "OK:" + result
  }

  @ExcelMethod
  @XLFunction(
    name = "uploadEquityPrices",
    category = "Starling",
    args = Array("label", "observationDay", "rics", "prices"))
  def uploadEquitiesHistory(label: String, observationDate: Double, rics: Array[Object], prices: Array[Object]) = {
    val observationPoint = ObservationPoint.fromExcel(observationDate)
    if (rics.size != prices.size) {
      throw new Exception("The ric and price ranges must be the same size")
    }
    val equityPrices = EquityPrices(TreeMap.empty[RIC, Quantity](RIC.ordering) ++ (rics zip prices).flatMap{
      case (ricOrBlank, priceOrBlank) => {
        (ricOrBlank, priceOrBlank) match {
          case (null, null) => None
          case ("", _) => None
          case (_, d: java.lang.Double) if d == 0 => None
          case (s: String, d: java.lang.Double) => {
            val ric = RIC(s)
            val price = {
              val value = d.doubleValue
              if (ric.currency == UOM.GBP) {
                value / 100
              } else {
                value
              }
            }
            Some(ric -> Quantity(price, ric.currency / UOM.SHARE))
          }
          case (_, _) => None
        }
      }
    })
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(marketDataSet, TimedMarketDataKey(observationPoint, EquityPricesMarketDataKey), equityPrices)
    "OK:" + result
  }


  @ExcelMethod
  @XLFunction(
    name = "uploadEquitiesHistory",
    category = "Starling",
    args = Array("label", "priceHistory"),
    argHelp = Array(
      "The name of the set of data to upload into",
      "A range containg the price history with RICs at the top and days on the left"))
  def uploadEquitiesHistory(label: String, priceHistory: Array[Array[Object]]) = {
    if (label == null) throw new Exception("Label is null")
    val ricHeader = Map() ++ priceHistory(0).toList.tail.zipWithIndex.flatMap{
      case (ricOrCurrency, index) => {
        ricOrCurrency match {
          case "" => None
          case s: String => Some((index + 1) -> RIC(s)) //+1 beacuse Day is column 0
          case _ => None
        }
      }
    }
    val processedPrices = priceHistory.toList.tail.flatMap{
      row => {
        row.head match {
          case d: java.lang.Double => {
            val observationPoint = ObservationPoint.fromExcel(d.doubleValue)
            val equityPrices = EquityPrices(TreeMap.empty[RIC, Quantity](RIC.ordering) ++ ricHeader.flatMap{
              case (index, ric) => {
                row(index) match {
                  case d: java.lang.Double => {
                    val currency = ric.currency
                    val price = {
                      val value = d.doubleValue
                      if (currency == UOM.GBP) {
                        value / 100
                      } else {
                        value
                      }
                    }
                    Some(ric -> Quantity(price, currency / UOM.SHARE))
                  }
                  case _ => None
                }
              }
            })
            Some(MarketDataEntry(observationPoint, EquityPricesMarketDataKey, equityPrices))
          }
          case _ => None
        }
      }
    }
    val marketDataSet = MarketDataSet.excel(label)
    val result = marketDataStore.save(Map(marketDataSet -> processedPrices))
    "OK:" + result
  }

  private def objectToDateRange(observationPoint: ObservationPoint, obj: Object, preferredTenor: TenorType = Day): Option[DateRange] = {
    val parsedRange = obj match {
      case d: java.lang.Double => Some(Day.fromExcel(d.doubleValue))
      case s: java.lang.String if (s.length > 0) => Some(TenorType.parseTenor(s))
      case _ => None
    }

    val parsed = parsedRange.map(r => r match {
      case d: Day => preferredTenor.containing(d)
      case _ => r
    })

    // HACK
    // this means we lose all forward prices before today except for the most recent one. which will be re-written
    // as having a day of observation day (if it is a daily market).
    // this isn't great, we should probably be working out what delivery they are for, and perhaps not interpolating
    // but this hack will do for now
    val observationDay = observationPoint.day.getOrElse(Day.today)
    preferredTenor match {
      case Day if parsed.get.asInstanceOf[Day] < observationDay => {
        Some(observationDay)
      }
      case _ => parsed
    }
  }
}