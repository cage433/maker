package starling.curves.readers

import starling.daterange._
import starling.db._
import starling.curves._
import starling.marketdata._
import starling.quantity.UOM._
import starling.utils.ImplicitConversions._
import starling.quantity.{Percentage, UOM, Quantity}
import starling.market._
import starling.calendar.BusinessCalendars
import collection.immutable.{Map, TreeMap}
import starling.dbx.QueryBuilder._
import starling.dbx.Query
import starling.pivot.MarketValue
import starling.daterange.ObservationPoint._
import starling.utils.MathUtil
import scala.unchecked

case class FwdCurveDbMarketDataSource(varSqlDB: DB, businessCalendars: BusinessCalendars, pricingGroupID: Int,
  override val marketDataSet: MarketDataSet) extends MarketDataSource {

  def table(market: CommodityMarket) = market.commodity match {
    case _: MetalCommodity => MetalsPriceTable
    case _ => NonMetalsPriceTable
  }

  def read(day: Day) = {
    val observationPoint = ObservationPoint(day)
    val observationDay = day

    import FwdCurveDbMarketDataSource._
    if(!anyPrices(varSqlDB, MetalsPriceTable, observationDay, pricingGroupID) && !anyPrices(varSqlDB, NonMetalsPriceTable, observationDay, pricingGroupID)) {
      throw new NoMarketDataForDayException(observationDay, "There is no market data in the Forward Curve Application for this observation day: " + (observationDay, this))
    }

    val spotFXKeys: List[SpotFXDataKey] = FwdCurveAppExternalMarketDataReader.currencyCurveIDs.keysIterator.map{ccy=>SpotFXDataKey(ccy)}.toList
    val priceKeys: List[PriceDataKey] = PriceDataType.keys.filter(m => m.market.eaiQuoteID.isDefined)
    val forwardRateKeys: List[ForwardRateDataKey] = List(ForwardRateDataKey(UOM.USD)) // TODO [05 Apr 2011] We only have USD at the moment. Fix this once we have more
    val volSurfaceKeys: scala.List[OilVolSurfaceDataKey] = OilVolSurfaceDataType.keys
    val spredStdKeys: List[SpreadStdDevSurfaceDataKey] = SpreadStdDevSurfaceDataType.keys
    val freightRateKeys = FreightFlatRateDataType.keys.filter(m => m.market.eaiQuoteID.isDefined)

    def readSomething[T](data: List[T], fn: T => (MarketDataKey, MarketData)): List[MarketDataEntry] = {
      data.flatMap { datum =>
        try {
          val (key, marketData) = fn(datum)
          Some(MarketDataEntry(observationPoint, key, marketData))
        } catch {
          case e:MissingMarketDataException => None
        }
      }
    }

    val range = (observationDay, observationDay)

    val spotMDEs = range.add(SpotFXDataType.name) → readSomething[SpotFXDataKey](spotFXKeys, SpotFXReader(observationDay).read)
    val prices = range.add(PriceDataType.name) → readSomething[PriceDataKey](priceKeys, PricesReader(observationDay).read)
    val volSurfaces = range.add(OilVolSurfaceDataType.name) → readSomething[OilVolSurfaceDataKey](volSurfaceKeys, OilVolSurfacesReader(observationDay).read)
    val spreadsStdDevs = range.add(SpreadStdDevSurfaceDataType.name) → readSomething[SpreadStdDevSurfaceDataKey](spredStdKeys, SpreadStdDevReader(observationDay).read)
    val freightRates = range.add(FreightFlatRateDataType.name) → readSomething[FreightFlatRateDataKey](freightRateKeys, FreightFlatRatesReader(observationDay).read)

    List(spotMDEs, prices, volSurfaces, spreadsStdDevs, freightRates).toMap
  }

  private def readSpreadStdDevSurfaceCurve(market: FuturesMarket, curveID : Int, observationDay : Day, pricingGroupID : Int) : PriceData = {

    val query = (
      select("p.price, p.intervalordinal")
              from (NonMetalsInputPriceTable + " p")
              where (
              ("p.curveid" eql curveID) and
              ("p.ObservationDate" eql observationDay) and
              ("p.PricingGroupID" eql 45) // this is coming from the input curve so hard coded to LDO group
              )
      )

    val prices = varSqlDB.queryWithResult(query) {
      rs => {
        val intervalOrdinal = rs.getInt("intervalordinal")
        val period = new Month(intervalOrdinal / 12, (intervalOrdinal % 12) + 1)
        val price = rs.getDouble("Price")
        period.asInstanceOf[DateRange] → Quantity(price, market.priceUOM)
      }
    }

    PriceData.fromMap(prices.toMap)
  }
  

  /**
   * There are some convoluted rules involving isCashDay that determine whether we want to include this price point
   */
  private def cashDayLogic(market: CommodityMarket) = (market.tenor : @unchecked) match {
    case Month => "and ((isCashDay = 0) or (isCashDay = 1 and isMonthDay = 1))"
    case Day => "and isCashDay = 0"
  }

  private def deliveryPeriod(market: CommodityMarket, rs: ResultSetRow): DateRange = {
    val forwardDay = rs.getDay("ForwardDate")
    (market, market.tenor) match {
      case (f:FuturesMarket, Month) => {
        // isMonthDay defines if the date is in as the last trading day or as just the start day of the delivery month
        rs.getInt("isMonthDay") match {
          case 1 => {
            val deliveryPeriod = f.frontPeriod(forwardDay)
            deliveryPeriod
          }
          case 0 => {
            Month(rs.getInt("forwardYear"), rs.getInt("intervalNumber"))
          }
        }

      }
      case (_, Day) => forwardDay
      case _ => {
        throw new Exception("Unknown market/tenor combo " + (market, market.tenor))
      }
    }
  }

  case class FreightFlatRatesReader(observationDay: Day) {
    def read(key: FreightFlatRateDataKey) = {

      val query = """
                      select FlatRate, ObservationDate
                      from EAI.dbo.tblFreightFlatRates
                      where quoteID = :quoteID
                      """
      val prices = varSqlDB.queryWithResult(query, Map("quoteID" -> key.market.eaiQuoteID.get)) {
        rs => {
          val price = Quantity(rs.getDouble("FlatRate"), FreightFlatRateForwardCurve.priceUOM).pq
          val year = rs.getDay("ObservationDate").containingYear
          (year, price)
        }
      }.toMap
      (key, new FreightFlatRateData(prices))
    }
  }

  case class SpotFXReader(observationDay: Day) {
    def read(sdk: SpotFXDataKey) = {
      val ccy = sdk.ccy
      val curveID = FwdCurveAppExternalMarketDataReader.currencyCurveIDs(ccy)
      val query = """
                      select Price
                      from %s
                      where CurveID = :CurveID
                            and PricingGroupID = :PricingGroupID
                           and ObservationDate = :ObservationDate
                           and ForwardDate = :ObservationDate
                      """ % (NonMetalsPriceTable) // TODO [05 Apr 2011] magic table knowledge

      varSqlDB.queryWithOneResult[MarketData](query, Map("CurveID" -> curveID, "PricingGroupID" -> 1, // TODO [05 Apr 2011] magic pricing group
        "ObservationDate" -> observationDay)) {
        rs => {
          SpotFXData(Quantity(rs.getDouble("price"), ccy / USD).invert.round(8)) //rounding because we only store 8 decimal places
        }
      } match {
      // TODO [05 Apr 2011] the SpotFXData object should contain an optional quantity - so as not to throw an exception
      // TODO [05 Apr 2011] until we actually need that rate
        case None => throw new MissingMarketDataException("Couldn't find spotfx for " + ccy + " on " + observationDay)
        case Some(rate: MarketData) => (sdk, rate)
      }
    }
  }

  case class PricesReader(observationDay: Day) {
    def read(pdk: PriceDataKey) = {
      val market = pdk.market match {
        case f: CommodityMarket => f
        case _ => throw new Exception("Only works for CommodityMarkets: " + pdk.market)
      }
      val eaiQuoteID = market.eaiQuoteID
      var pricePoints = TreeMap.empty[DateRange, Double](DateRange.ordering)
      val query = """
                      select ForwardDate, Price, ForwardYear, IntervalNumber, isMonthDay, isCashDay
                      from %s
                      where EAIQuoteID = :EAIQuoteID
                            and PricingGroupID = :PricingGroupID
                           and ObservationDate = :ObservationDate
                           and ForwardDate is not null
                           %s
                      """ % (table(market), cashDayLogic(market))

      var error = ""
      try {
        varSqlDB.query(query, Map("EAIQuoteID" -> eaiQuoteID.get, "PricingGroupID" -> pricingGroupID, "ObservationDate" -> observationDay)) {
          rs => {
            val delivery = deliveryPeriod(market, rs)

            val price = market.priceUOM match {
              case PERCENT => Percentage.fromPercentage(rs.getDouble("Price")).toQuantity
              case _ => {
                // FC app always has the price in a base currency. e.g. Rbob is in USD/GAL but should be in C/GAL
                val fcaPriceUOM = market.currency.inBaseCurrency / market.uom
                val priceUOM = market.priceUOM // correct unit
                Quantity(rs.getDouble("Price"), fcaPriceUOM) inUOM priceUOM
              }
            }
            pricePoints.get(delivery) match {
              case Some(p) if p != price.value => {
                error = "Failed reading for " + pdk + " on " + observationDay + " multiple date entries: " + delivery
              }
              case _ =>
            }
            pricePoints += (delivery -> price.value)
          }
        }
      }
      catch {
        case e => {
          throw e
        }
      }
      if (pricePoints.isEmpty) {
        throw new MissingMarketDataException("No forward curve prices for " + market.name + "(" + market.eaiQuoteID + ") pg: " + pricingGroupID + " on " + observationDay)
      }
      if (error.nonEmpty) {
        throw new MissingMarketDataException(error)
      }
      (pdk, PriceData.create(pricePoints, market.priceUOM))
    }
  }

  case class OilVolSurfacesReader(observationDay: Day) {
    def read(vsk : OilVolSurfaceDataKey) = {
      import starling.dbx.QueryBuilder._

      val query = (select ("c.Delta, p.ForwardDate, p.Price")
        from ("%s c" % NonMetalsCurvesTable)
        innerJoin ("%s p" % NonMetalsPriceTable, ("c.id" eql "p.CurveID"))
        where (
          ("c.VolatilityID" eql vsk.market.volatilityID.get)
          and ("p.ObservationDate" eql observationDay)
          and ("p.PricingGroupID" eql pricingGroupID)
        ))

      def forwardDateToPeriod(forwardDate : Day) : DateRange = vsk.market.tenor match {
        case Day => forwardDate
        case Month => forwardDate.containingMonth
      }
      var atmVolMap = Map[DateRange, Percentage]()
      var skewMap = Map[(Double, DateRange), Percentage]()
      varSqlDB.query(query){
        rs =>
          val period = forwardDateToPeriod(rs.getDay("ForwardDate"))
          // EAI Stores percentages as decimals
          val volatility = (rs.getPercentage("Price") * 100.0)
          if (rs.isNull("delta")){
            atmVolMap += period -> volatility
          } else {
            val delta = rs.getDouble("delta")
            skewMap += (delta, period) -> volatility
          }
      }

      if (skewMap.isEmpty) {
        throw new MissingMarketDataException
      }

      val periods : Array[DateRange] = (Set[DateRange]() ++ skewMap.keysIterator.map(_._2)).toList.sortWith(_<_).toArray
      val deltas : Array[Double] = (Set[Double]() ++ skewMap.keysIterator.map(_._1)).toList.sortWith(_<_).toArray
      val skewArray = deltas.map{d => periods.map{p => skewMap((d, p))}}

      if(periods.isEmpty) {
        throw new MissingMarketDataException("No oil vols for " + vsk.market.name + "(" + vsk.market.eaiQuoteID + ") pg: " + pricingGroupID + " on " + observationDay)
      }

      (vsk, new OilVolSurfaceData(periods, periods.map(atmVolMap(_)), deltas, skewArray))
    }
  }

  case class SpreadStdDevReader(observationDay: Day) {
    def read(ssdk : SpreadStdDevSurfaceDataKey) = {
      FwdCurveAppExternalMarketDataReader.constructSpreadStdDev(ssdk.market, ssdk.market.priceUOM,
        (curveIDs : List[Int]) => {
          Some(curveIDs.map(id => readSpreadStdDevSurfaceCurve(ssdk.market, id, observationDay, pricingGroupID)))
        }) match {
        case Some(stdDevs) => (ssdk, stdDevs)
        case None => {
          throw new MissingMarketDataException("Missing spread standard deviation surface for " + ssdk.market + " on " + observationDay + ".")
        }
      }
    }
  }
}

object FwdCurveDbMarketDataSource {
  def anyPrices(varSqlDB: DB, table: ForwardCurvePricingTable, observationDay: Day, pricingGroupID: Int) = {
    val query: Query = (select("top 1 *") from (table.toString) where
      (("pricingGroupID" eql pricingGroupID) and
        ("observationdate" eql observationDay) and
        ("eaiQuoteID" isNotNull) and ("eaiQuoteID" neq 987)  // hack, because LDO always has entries in tblOutputPrices
        )
      )
    val res = varSqlDB.queryWithOneResult(query) {
      rs => rs.getInt("id")
    }
    res isDefined
  }
}
