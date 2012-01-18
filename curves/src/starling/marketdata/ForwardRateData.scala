package starling.marketdata

import starling.utils.ImplicitConversions._

import starling.pivot._
import scalaz.NewType
import utils.TenorPivotParser
import scalaz.Scalaz._
import starling.quantity.{UOMType, Quantity, UOM}
import starling.metals.datasources.LIBORCalculator
import starling.daterange._
import java.lang.String


object ForwardRateDataType extends MarketDataType {
  type dataType = ForwardRateData
  type keyType = ForwardRateDataKey
  val keys = UOM.currencies.map(s => ForwardRateDataKey(s))
  val humanName = "forward rates"

  def extendedKeys = List(currencyField)
  override def valueKeys = List(publisherField, tenorField)
  def valueFieldDetails = List(rateField)

  val publisherField = FieldDetails("Publisher")
  val currencyField = FieldDetails.list("Currency", UOM.currencies.map(_.asString))
  val tenorField = new FieldDetails("Tenor") {
    override def parser = TenorPivotParser
    override def comparator = new OrderedOrdering[Tenor].untyped
  }
  val rateField = FieldDetails.createMeasure("Rate", PivotQuantitySetPivotFormatter, PivotQuantityPivotParser)

  def createKey(row: Row) = ForwardRateDataKey(UOM.parseCurrency(row.string(currencyField)))

  def createValue(rows: List[Row]) = ForwardRateData(rows.map { row =>
    (ForwardRatePublisher(row.string(publisherField)), (Tenor.parse(row(tenorField)), row.quantity(rateField)))
  }.toNestedMap)

  val initialPivotState = PivotFieldsState(
    rowFields = List(Field("Observation Time"), publisherField.field, currencyField.field, tenorField.field),
    dataFields = List(rateField.field)
  )

  protected def fieldValuesFor(key: ForwardRateDataKey) = Row(currencyField.field → key.ccy.asString)

  def rows(key: ForwardRateDataKey, data: ForwardRateData) = data.rates.mapNested { case (source, tenor, rate) =>
    fieldValuesFor(key) + Row(publisherField.field → source.value, tenorField.field → tenor, rateField.field → rate.pq)
  }
}

case class ForwardRateDataKey(ccy: UOM) extends MarketDataKey {
  ccy.ensuring(_.isCurrency, ccy + " is not a currency")

  type marketDataType = ForwardRateData
  def typeName = ForwardRateDataType.name
  def humanName = ccy.toString
  def fields = Set(ForwardRateDataType.currencyField.field)
  def observationTime = (ccy == UOM.CNY) ? ObservationTimeOfDay.CFETSPublicationTime | ObservationTimeOfDay.LiborClose
  def onDay(observationDay: Day) = TimedMarketDataKey(observationDay.atTimeOfDay(observationTime), this)
}

case class ForwardRatePublisher(value: String, currencies: UOM*) extends NewType[String] {
  import Tenor._
  def tenors: List[Tenor] = (this == ForwardRatePublisher.IRS) ? Tenor.many(Year, 1, 2, 3, 4, 5, 10, 20) |
    ON :: SN :: OneYear :: Tenor.many(Week, 1, 2) ::: Tenor.many(Month, Range(1, 13) : _*)
}

object ForwardRatePublisher {
  import UOM._

  val LIBOR = ForwardRatePublisher("LIBOR", AUD, CAD, CHF, EUR, GBP, JPY, NZD, USD)
  val IRS = ForwardRatePublisher("IRS", AUD, CAD, CHF, EUR, GBP, JPY, USD)
  val BUBOR = ForwardRatePublisher("BUBOR", BGN)
  val HIBOR = ForwardRatePublisher("HIBOR", HKD)
  val JIBAR = ForwardRatePublisher("JIBAR", ZAR)
  val KLIBOR = ForwardRatePublisher("KLIBOR", MYR)
  val ROBOR = ForwardRatePublisher("ROBOR", RON)
  val SHIBOR = ForwardRatePublisher("SHIBOR", SGD)
  val SIBOR = ForwardRatePublisher("SIBOR", SGD)
  val TRLIBOR = ForwardRatePublisher("TRLIBOR", TRY)

  val values = List(LIBOR, IRS, BUBOR, HIBOR, JIBAR, KLIBOR, ROBOR, SHIBOR, SIBOR, TRLIBOR)
  private val valuesByName = values.toMapWithKeys(_.value)

  def fromName(name: String): ForwardRatePublisher = valuesByName(name)
}

case class ForwardRateData(rates: NestedMap[ForwardRatePublisher, Tenor, Quantity]) extends MarketData {
  require(rates.keySet.forall(_.isInstanceOf[ForwardRatePublisher]))
  require(rates.flipNesting.keySet.forall(_.isInstanceOf[Tenor]))
  require(rates.allValues.forall(_.isInstanceOf[Quantity]))

  def lastDay(fixingDay: Day): Day = fixingDay
  def size = rates.nestedSize
  def ratesFor(publishers: ForwardRatePublisher*): Map[Tenor, Quantity] = publishers.toList.flatMapO(rates.get).head
}