package starling.curves

import starling.quantity.Quantity
import cern.colt.matrix.impl.{DenseDoubleMatrix1D => DVector, DenseDoubleMatrix2D => DMatrix}
import starling.utils.conversions.RichColtMatrices._
import cern.colt.matrix.{DoubleFactory1D, DoubleFactory2D, DoubleMatrix1D => Vector, DoubleMatrix2D => Matrix}
import collection.immutable.List
import cern.colt.matrix.linalg.{LUDecomposition, Algebra}
import starling.utils.ImplicitConversions._
import starling.pivot.{MarketValue, PivotQuantity}
import starling.daterange._
import starling.marketdata.PriceData._
import starling.marketdata.PriceFixingsHistoryDataKey._
import starling.marketdata.TimedMarketDataKey._
import starling.marketdata.PriceDataKey._
import starling.market.{Index, PublishedIndex}
import starling.db.MarketDataReader
import starling.marketdata._

case class Months(months : List[Month]){
  def - (rhs : Months) : Months = {
    Months(months.filterNot(rhs.months.contains))
  }
  def intersects(rhs : Months) : Boolean = months.exists(rhs.months.contains)

  def disjunct(rhs : Months) = List(this - rhs, rhs - this, intersection(rhs)).filterNot(_.isEmpty)

  def intersection(rhs : Months) = Months(months.filter(rhs.months.contains))

  def isEmpty = months.isEmpty

  def size = months.size

  def contains (rhs : Months) = months.forall(rhs.months.contains)
}

object Months{
  def minimallyDisjoint(months : List[Months]) : List[Months] = {
    months match {
      case Nil => Nil
      case head :: Nil => months
      case head :: tail => tail.find(head.intersects) match {
        case None => head :: minimallyDisjoint(tail)
        case Some(rhs) => minimallyDisjoint( head.disjunct(rhs) ::: tail.filterNot(_ == rhs))
      }
    }
  }
}

class ProblemBuilder(dim : Int){
  private var matrix : Matrix = new DMatrix(0, dim)
  private var vector : Vector = new DVector(0)
  private def rank = matrix.rows
  private def addSingleRow(v: Vector): Matrix = {
    DoubleFactory2D.dense.appendRows(matrix, v.toRowMatrix)
  }

  def isOrthogonal(v : Vector) : Boolean = {
    Algebra.DEFAULT.rank(addSingleRow(v).viewDice) == rank + 1
  }

  def add(v : Vector, p : Double){
    matrix = addSingleRow(v)
    vector = DoubleFactory1D.dense.append(vector, new DVector(1).assign(p))
  }
  def isComplete() = rank == dim

  def solve : Vector = {
    new LUDecomposition(matrix).solve(vector.toColumnMatrix).viewColumn(0)
  }
  def print{
    println("Matrix = \n" + matrix)
    println("vector = \n" + vector)
  }
}

object FreightCurve {

  val freightMarketsWhichHavePrices = List(
    "Baltic Supramax T/C Avg",
    "Panamax T/C Average (Baltic)",
    "Capesize T/C Average (Baltic)").map(Index.publishedIndexFromName)


  /**
   * Returns PriceData for the Freight markets we support. Note that Freight prices are published
   * as CURMOM, CAL1 etc. This method turns then into date range
   */
  def freightPriceData(observationDay: Day, marketDataReader: MarketDataReader) = {
    freightMarketsWhichHavePrices.map { market =>
      val freightKey = PriceFixingsHistoryDataKey(market)
      val freightPrices = marketDataReader.read(
        TimedMarketDataKey(observationDay.atTimeOfDay(ObservationTimeOfDay.LondonClose), freightKey)
      ).asInstanceOf[PriceFixingsHistoryData]
      PriceDataKey(market) -> FreightCurve.tenorsToPriceData(market, observationDay, freightPrices)
    }
  }

  def tenorsToPriceData(market:PublishedIndex, observationDay:Day, priceFixingTenors:PriceFixingsHistoryData) = {
    val nextFloatingDay: Day = observationDay.nextBusinessDay(market.businessCalendar)
    val p = priceFixingTenors.fixings.map {
      case ( (_, storedFixingPeriod), MarketValue.Quantity(price) ) => storedFixingPeriod.toDateRange(nextFloatingDay) → price.pq
    }
    PriceData(p.toMap)
  }

  // Create a reasonable monthly price map from a mixture of months, quarters and years. Note that discounting is ignored.
  def calcMonthlyPricesFromArbitraryPeriods(rawPrices: Map[DateRange, PivotQuantity]): Map[Month, Quantity] = if (rawPrices.isEmpty) {
    Map()
  } else {
    val priceUOM = rawPrices.head._2.quantityValue.get.uom

    calcMonthlyPricesFromArbitraryPeriodsWithUnusedPeriodList(rawPrices.mapValues(_.quantityValue.get.value))
      ._1.mapValues(value => Quantity(value, priceUOM))
  }

  // For unit tests only. Gives the periods that were ignored through arbitrage
  def calcMonthlyPricesFromArbitraryPeriodsWithUnusedPeriodList(rawPrices: scala.collection.Map[DateRange, Double]):
    (Map[Month, Double], List[DateRange]) = {

    if (rawPrices.isEmpty) return (Map[Month, Double](), List[DateRange]())

    val periodMonths: Map[DateRange, Months] = rawPrices.keySet.map{dr => dr → Months(dr.toListOfMonths)}.toMap
    val disjointMonths: List[Months] = Months.minimallyDisjoint(periodMonths.valuesIterator.toList)
    def vectorRepresentation(months: Months): Vector = new DVector(disjointMonths.size).updateIt { vector =>
      disjointMonths.zipWithIndex.foreach { case (lhs, i) =>
        if (months.intersects(lhs)) vector(i) = 1.0
      }
    }

    val pb = new ProblemBuilder(disjointMonths.size)
    var ignoredPeriods = List[DateRange]()
    val periodsBySize: List[(DateRange, Months)] = periodMonths.toList.sortWith(_._2.size < _._2.size)
    periodsBySize.foreach { case (period, months) =>
      val v = vectorRepresentation(months)

      if (pb.isOrthogonal(v)){
        pb.add(v, rawPrices(period) * months.size)
      } else {
        ignoredPeriods ::= period
      }
    }
    def representativePrice(months: Months) = periodsBySize.find { case (period, rMonths) => rMonths.contains(months) } match {
      case Some((period, _)) => rawPrices(period)
      case None => throw new Exception("Should never happen - written to remove compiler warning")
    }
    disjointMonths.sortWith(_.size < _.size).toStream.takeWhile(_ => ! pb.isComplete()).foreach { months =>
      val v = vectorRepresentation(months)

      if (pb.isOrthogonal(v)) {
        pb.add(v, representativePrice(months) * months.size)
      }
    }

    val solved = pb.solve.toArray

    val monthlyPrices = disjointMonths.zip(solved).flatMap { case (months, price) => months.months.map(_ → (price / months.size)) }

    (monthlyPrices.toMap, ignoredPeriods)
  }
}
