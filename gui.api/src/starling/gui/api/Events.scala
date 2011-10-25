package starling.gui.api

import swing.event.Event
import starling.auth.User
import starling.utils.ImplicitConversions._
import java.lang.String
import collection.immutable.Map
import starling.pivot.PivotLayout
import starling.daterange._
import starling.quantity.UOM
import starling.browser.service.StarlingGUIEvent

class Events //This is just here as I find this class using "^n Events"

case class IntradayUpdated(group: String, user: User, timestamp:Timestamp) extends StarlingGUIEvent
case class TradesUpdated(desk : Desk, timestamp :Timestamp) extends StarlingGUIEvent
case class DeskClosed(desk: Desk, timestamp:TradeTimestamp) extends StarlingGUIEvent
case class DeskCloseFailed(desk: Desk, timestamp:TradeTimestamp, error: Throwable) extends StarlingGUIEvent
case class MarketDataSnapshot(snapshotID:SnapshotIDLabel) extends StarlingGUIEvent
case class PricingGroupMarketDataUpdate(pricingGroup:PricingGroup, version:Int,
                                        previousVersion:Int, affectedObservationDays: List[Day]) extends StarlingGUIEvent
case class ExcelObservationDay(name:String, day:Day) extends StarlingGUIEvent
case class PricingGroupObservationDay(pricingGroup:PricingGroup, day:Day) extends StarlingGUIEvent
case class PivotLayoutUpdate(user:String, userLayouts:List[PivotLayout]) extends StarlingGUIEvent
case class ExcelMarketListUpdate(values:List[String]) extends StarlingGUIEvent
case class ExcelMarketDataUpdate(name:String, version:Int) extends StarlingGUIEvent
case class TestEvent(text:String) extends StarlingGUIEvent
case class RabbitEventReceived(latestTimestamp:Long) extends StarlingGUIEvent
case class RefinedMetalsValuationChanged(observationDay:Day, snapshotID:SnapshotIDLabel, changedTrades:Set[String]) extends Event

object PricingGroupMarketDataUpdate {
  def matching(pricingGroupOption : Option[PricingGroup]) : PartialFunction[Event, PricingGroupMarketDataUpdate] = {
    pricingGroupOption.map(pricingGroup => matching(pricingGroup))
  }

  def matching(pricingGroup : PricingGroup) : PartialFunction[Event, PricingGroupMarketDataUpdate] = {
    case update@PricingGroupMarketDataUpdate(`pricingGroup`, _, _, _) => update
  }
}

object ExcelMarketDataUpdate {
  def matching(nameOption : Option[String]): PartialFunction[Event, ExcelMarketDataUpdate] = {
    nameOption.map(name => matching(name))
  }

  def matching(name : String) : PartialFunction[Event, ExcelMarketDataUpdate] = {
    case update@ExcelMarketDataUpdate(`name`, _) => update
  }
}

case class RabbitMessage(body:String, headers:Map[String,Object])

abstract class RabbitEvent(val queueName : String) extends Event {
  import sjson.json._

  def toMessage = RabbitMessage(toJSON, toMap("userName", "subGroupName"))
  def toJSON = new String(Serializer.SJSON.out(toMap()))
  def toMap(keys : String*) : Map[String, Object] = toMap & keys.toSet
  def toMap : Map[String, Object]

  protected def priceEventMap(user : User, label : String, observationDate : Option[Day], dates : Array[_]) = Map(
    "userName" -> user.username,
    "label" -> label,
    "observationDate" -> observationDate.map(_.toString("dd MMM yyyy")).getOrElse(""),
    "dates" -> dates.map(_.toString)
  )
}

case class BlotterTradeUpdate(user : User, subGroupName : String, data : List[List[String]])
  extends RabbitEvent("Trafigura.Raw.Trade.RiskManagement") {

  def toMap = Map("userName" → user.username, "subGroupName" → subGroupName, "data" → data)
}

case class UploadPricesUpdate(user : User, label : String, observationPoint : ObservationPoint, dates : Array[Day],
                              marketName : String, prices : Array[Double])
  extends RabbitEvent("Trafigura.Raw.Price.RiskManagement") {

  def toMap = priceEventMap(user, label, observationPoint.day, dates) ++
    Map("marketName" → marketName, "prices" → prices.map(_.toString))
}

case class UploadStandardDeviationsUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Period],
                                          marketName : String, standardDeviations : Array[Array[Double]])
  extends RabbitEvent("Trafigura.Raw.StandardDeviations.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("marketName" → marketName, "standardDeviations" → standardDeviations.map(_.map(_.toString)))
}

case class UploadVolsUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Day],
                            marketName : String, vols : Array[Array[Double]])
  extends RabbitEvent("Trafigura.Raw.Vols.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("marketName" → marketName, "vols" → vols.map(_.map(_.toString)))
}

case class UploadInterestRatesUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Day],
                                     currency : String, interestRates : Array[Double])
  extends RabbitEvent("Trafigura.Raw.InterestRates.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("currency" → currency, "interestRates" → interestRates.map(_.toString))
}

case class Email(from: String = "", to: String = "", subject: String = "", body: String = "", footers: Map[String, String] = Map()) {
  val hash = (from + to + subject + body).md5

  def +(footer: (String, String)) = copy(footers = this.footers + footer)
  def +(footers: Map[String, String]) = copy(footers = this.footers ++ footers)

  def bodyWithFooters = "<html><body>" + body + "\n\n<br/><br/><i>" + footers.map(kv => "%s: %s" % (kv._1, kv._2)).mkString(", ") + "</i>" + "</body></html>"
}

case class EmailEvent(from: String = "", to: String = "", subject: String = "", body: String = "") extends Event {
  val email = Email(from, to, subject, body)
}

case class EmailSent(timeSent: Timestamp) extends StarlingGUIEvent

abstract class MarketDataEvent(val observationDay: Day, val label: SnapshotIDLabel, isCorrection: Boolean) extends Event
case class SpotFXDataEvent(override val observationDay: Day, currencies: List[UOM],
                           override val label: SnapshotIDLabel, isCorrection: Boolean)
  extends MarketDataEvent(observationDay, label, isCorrection)

case class ReferenceInterestRateDataEvent(override val observationDay: Day, exchange: String, currencies: List[UOM],
                                          override val label: SnapshotIDLabel, isCorrection: Boolean)
  extends MarketDataEvent(observationDay, label, isCorrection)

case class PriceDataEvent(override val observationDay: Day, market: String, periods: List[DateRange],
                          override val label: SnapshotIDLabel, isCorrection: Boolean)
  extends MarketDataEvent(observationDay, label, isCorrection)

