package starling.marketdata

import starling.utils.ImplicitConversions._
import starling.db.MarketDataReader
import starling.curves.MissingMarketDataException
import starling.quantity.Quantity
import starling.daterange.{DateRange, Day, ObservationPoint}
import starling.pivot.PivotQuantity


trait PriceValidator {
  def validate(timedKey: TimedMarketDataKey, priceData: PriceData): PriceData

  def +(that: PriceValidator): PriceValidator = (this, that) match {
    case (PriceValidator.Null, _) => that
    case (_, PriceValidator.Null) => this
    case _ => new CompositePriceValidator(List(this, that))
  }
}

object PriceValidator {
  object Null extends PriceValidator {
    def validate(timedKey: TimedMarketDataKey, priceData: PriceData) = priceData
  }
}

class CompositePriceValidator(validators: Seq[PriceValidator]) extends PriceValidator {
  def validate(timedKey: TimedMarketDataKey, priceData: PriceData): PriceData = {
    val vs = validators.map(validator => (validator.validate _).curried(timedKey))

    priceData.applyAll(vs : _*)
  }
}

object MeanPriceValidator extends PriceValidator {
  def validate(timedKey: TimedMarketDataKey, priceData: PriceData) = {
    val prices = priceData.prices
    lazy val mean = prices.values.map(_.quantityValue.get).sum / prices.size

    PriceData(prices.mapValues { price => {
      val difference = price.quantityValue.get percentageDifference mean
      if (difference > 0.1) {
        price.copy(warning = Some("mean: " + mean + ", percent diff " + difference.toShortString + " > 10%"))
      } else {
        price
      }
    } })
  }
}

object RollingAveragePriceValidator extends PriceValidator {
  def validate(timedKey: TimedMarketDataKey, priceData: PriceData) = {
    val prices: List[(DateRange, PivotQuantity)] = priceData.prices.toList.sortBy(_._1)
    val values = prices.map(_._2.quantityValue.get.value)

    val newPrices = movingAverage(values, 3).zip(prices).map { case (rollingAverage, (date, price) ) => {
      val average = Quantity(rollingAverage, price.quantityValue.get.uom)

      val difference = price.quantityValue.get percentageDifference average
      if (difference > 0.1) {
        date → price.copy(warning = Some("moving average: " + average + ", percent diff " + difference.toShortString + " > 10%"))
      } else {
        date → price
      }
    } }.toMap

    priceData.copy(newPrices)
  }

  def movingAverage(vs: List[Double], window: Int): List[Double] = {
    if (window > vs.size) {
      vs
    } else {
      val averages = ((vs.take(window).sum / window :: Nil, vs) /: vs.drop(window)) {(a, v) =>
        ((a._1.head - a._2.head / window + v / window) :: a._1, a._2.tail)
      }._1.reverse

      val toPad = vs.size - averages.size
      val headPad = (toPad.doubleValue / 2.0).ceil
      val lastPad = (toPad.doubleValue / 2.0).floor

      List.fill(headPad.toInt)(averages.head) ::: averages ::: List.fill(lastPad.toInt)(averages.last)
    }
  }
}

class DayChangePriceValidator(reader: MarketDataReader) extends PriceValidator {
  def anotate(previous: PriceData, current: PriceData): PriceData = {

    val anotatedPrices = current.prices.map {
      case (period, currentPrice) => period → {
        val previousPrice = previous.prices.getOrElse(period, currentPrice).quantityValue.get
        val currentQuantity = currentPrice.quantityValue.get

        if (currentQuantity.uom != previousPrice.uom) {
          currentPrice.copy(warning = Some("unit has changed from: " + previousPrice.uom))
        } else {
          val difference = previousPrice percentageDifference currentQuantity

          if (difference > 0.2) {
            currentPrice.copy(warning = Some("percent diff to " + previousPrice.value + " > 20%"))
          } else {
            currentPrice
          }
        }
      }
    }

    PriceData(anotatedPrices.toMap)
  }

  def validate(timedKey: TimedMarketDataKey, current: PriceData) = {
    timedKey.day match {
      case Some(day) => readPrevious(day, timedKey.key).map(anotate(_, current)).getOrElse(current)
      case _ => current
    }
  }

  private def readPrevious(day: Day, key: MarketDataKey): Option[PriceData] = {
    try {
      val recentDays: Set[Option[Day]] = (day - 5 until day).map(d => Some(d)).toSet
      val days = reader.read(PriceDataType, Some(recentDays), None, Some(Set(key))).sortBy(_.head.day)
      days.lastOption.map(_.last.asInstanceOf[PriceData])
    } catch {
      case e: MissingMarketDataException => None
    }
  }
}