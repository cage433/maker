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

case class FwdCurveDbMarketDataSource(varSqlDB: DB, businessCalendars: BusinessCalendars, pricingGroupID: Int) extends MarketDataSource {

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
    val indexes: List[SingleIndex] = Index.indicesToImportFixingsForFromEAI.filter { index => {
      index match {
        case publishedIndex: PublishedIndex => publishedIndex.market.limSymbol.isEmpty
        case futuresFrontPeriodIndex: FuturesFrontPeriodIndex => futuresFrontPeriodIndex.market.limSymbol.isEmpty
        case _ => true
      }
    } }
    val forwardRateKeys: List[ForwardRateDataKey] = List(ForwardRateDataKey(UOM.USD)) // TODO [05 Apr 2011] We only have USD at the moment. Fix this once we have more
    val volSurfaceKeys: scala.List[OilVolSurfaceDataKey] = OilVolSurfaceDataType.keys
    val spredStdKeys: List[SpreadStdDevSurfaceDataKey] = SpreadStdDevSurfaceDataType.keys

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

    val spotMDEs = range.add(SpotFXDataType) → readSomething[SpotFXDataKey](spotFXKeys, SpotFXReader(observationDay).read)
    val prices = range.add(PriceDataType) → readSomething[PriceDataKey](priceKeys, PricesReader(observationDay).read)
    val fixings = FixingsReader(observationDay).read(indexes)
//    val fixings = range.add(PriceFixingsHistoryDataType)  → readSomething[SingleIndex](indexes, FixingsReader(observationDay).read)
    val forwardRates = range.add(ForwardRateDataType) → readSomething[ForwardRateDataKey](forwardRateKeys, ForwardRateReader(observationDay).read)
    val volSurfaces = range.add(OilVolSurfaceDataType) → readSomething[OilVolSurfaceDataKey](volSurfaceKeys, OilVolSurfacesReader(observationDay).read)
    val spreadsStdDevs = range.add(SpreadStdDevSurfaceDataType) → readSomething[SpreadStdDevSurfaceDataKey](spredStdKeys, SpreadStdDevReader(observationDay).read)

    List(spotMDEs, prices, fixings, forwardRates, volSurfaces, spreadsStdDevs).toMap
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
  private def cashDayLogic(market: CommodityMarket, index: Option[Index] = None) = (market.tenor, index) match {
    case (Month,_) => "and ((isCashDay = 0) or (isCashDay = 1 and isMonthDay = 1))"
    case (_,Some(_: PublishedIndex)) => "and isCashDay = 0"
    case _ => "and isCashDay in (0,1)"
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

  case class ForwardRateReader(ignoredObservationDay: Day) {
    def read(frdk: ForwardRateDataKey) = {
      val ccy = frdk.ccy
      val curveID = FwdCurveAppExternalMarketDataReader.forwardRateCurveIDs(ccy)
      val query = """
                      select ForwardDate, Price
                      from %s
                      where CurveID = :CurveID
                            and PricingGroupID = 1
                           and ObservationDate = '2010-01-26'
                      """ % (NonMetalsPriceTable) // TODO [05 Apr 2011] magic table knowledge
      // TODO [05 Apr 2011] hard coded observation date until we get full data from LIM

      val entries = varSqlDB.queryWithResult(query, Map("CurveID" -> curveID, "PricingGroupID" -> pricingGroupID)) {
        rs => {
          // TODO [05 Apr 2011] this needs to be looked at when we have real data in the FCA
          new ForwardRateDataEntry(rs.getDay("ForwardDate"), "Annual", "SWAP", rs.getDouble("price"))
        }
      }
      (frdk, ForwardRateData(entries))
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

  case class FixingsReader(observationDay: Day) {
    def read(indexes: List[SingleIndex]): ((Day, Day, MarketDataType), List[MarketDataEntry]) = {

      val a = indexes.flatMap { index => {
        val eaiQuoteID : Option[Int] = index match {
          case p:PublishedIndex => p.eaiQuoteID
          case _ => index.market.eaiQuoteID
        }
        val cashLogic = cashDayLogic(index.market, Some(index))
        eaiQuoteID.map(eai => (index.market, (eai, cashLogic, index.level)))
      }}.toMultiMap

      val entries = a.flatMap { case (market, otherDetails) => readSingle(otherDetails, market) }

      (observationDay.startOfFinancialYear, observationDay, PriceFixingsHistoryDataType) → entries.toList
    }

    def readSingle(otherDetails: Traversable[(Int, String, Level)], market: CommodityMarket): List[MarketDataEntry] = {
      (otherDetails.toSet.toList match {
        case (eaiQuoteID, cash, level) :: Nil => readSingle(market, cash, level, eaiQuoteID)
        case rules => throw new Exception("This market has more than one eai/cash rule " + market + ", " + rules)
      })
        .toMultiMap.mapValues(PriceFixingsHistoryData.sum)
        .map { case (day, data) => MarketDataEntry(ObservationPoint(day), PriceFixingsHistoryDataKey(market), data) }.toList
        .update(duplicateTimedKeys(_).require(_.isEmpty, "Duplicate 'timed' keys: "))
    }

    def readSingle(market:CommodityMarket, crazyCashLogic:String, level:Level, eaiQuoteID:Int): List[(Day, PriceFixingsHistoryData)] = {
      val tableName = table(market)

      varSqlDB.queryWithResult(
        """     select op.CurveID, op.ObservationDate, op.ForwardDate, op.Price, op.ForwardYear, op.IntervalNumber, op.isMonthDay from
                %s op
        join
                (select EAIQuoteID, ObservationDate, min(ForwardDate) as MinForwardDate
                        from %s
                        where
                        EAIQuoteID = :EAIQuoteID
                        and PricingGroupID = :PricingGroupID
                        and ObservationDate >= :DateFrom
                        and ObservationDate <= :DateTo
                        %s
                        group by EAIQuoteID, ObservationDate) op2
        on
                op.EAIQuoteID = op2.EAIQuoteID and op.ObservationDate = op2.ObservationDate and op.ForwardDate = op2.MinForwardDate
        where
                op.EAIQuoteID = :EAIQuoteID
                and op.PricingGroupID = :PricingGroupID
        order by op.ObservationDate

        """ % (tableName, tableName, crazyCashLogic)
        , Map("EAIQuoteID" → eaiQuoteID,
              "PricingGroupID" → pricingGroupID,
              "DateFrom" → observationDay.startOfFinancialYear,
              "DateTo" -> observationDay))
      {
        rs => {
          val observationDate = rs.getDay("ObservationDate")
          val delivery = deliveryPeriod(market, rs)
          val price = MarketValue.quantity(MathUtil.roundToNdp(rs.getDouble("Price"), 8), market.priceUOM)

          (observationDate, PriceFixingsHistoryData.create(Map((level, StoredFixingPeriod.dateRange(delivery)) → price)))
        }
      }
    }

    private def duplicateTimedKeys(entries: List[MarketDataEntry]) = entries.map(_.timedKey).duplicates
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


      try {
        varSqlDB.query(query, Map("EAIQuoteID" -> eaiQuoteID.get, "PricingGroupID" -> pricingGroupID, "ObservationDate" -> observationDay)) {
          rs => {
            val delivery = deliveryPeriod(market, rs)

            // FC app always has the price in a base currency. e.g. Rbob is in USD/GAL but should be in C/GAL
            val fcaPrimeUOM = market.currency.inBaseCurrency / market.uom
            val priceUOM = market.priceUOM // correct unit
            val price = Quantity(rs.getDouble("Price"), fcaPrimeUOM) inUOM priceUOM
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
