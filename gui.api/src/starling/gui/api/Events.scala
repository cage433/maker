package starling.gui.api

import swing.event.Event
import starling.auth.User
import starling.utils.ImplicitConversions._
import java.lang.String
import collection.immutable.Map
import starling.daterange.{ObservationPoint, Day, Timestamp}
import starling.pivot.{ExtraFormatInfo, PivotLayout}
import starling.gui.Key

class Events //The is just here as I find this class using "^n Events"

case class EventBatch(events:Seq[Event]) extends Event //Needed so that many events are processed with one auto refresh

case class IntradayUpdated(group: String, user: User, timestamp:Timestamp) extends Event
case class DeskClosed(desk: Desk, timestamp:TradeTimestamp) extends Event
case class DeskCloseFailed(desk: Desk, timestamp:TradeTimestamp, error: Throwable) extends Event
case class MarketDataSnapshot(snapshots:Map[MarketDataSelection,List[SnapshotIDLabel]]) extends Event
case class PricingGroupMarketDataUpdate(pricingGroup:PricingGroup, version:Int) extends Event
case class ExcelObservationDay(name:String, day:Day) extends Event
case class PricingGroupObservationDay(pricingGroup:PricingGroup, day:Day) extends Event
case class PivotLayoutUpdate(user:String, userLayouts:List[PivotLayout]) extends Event
case class UserReportUpdate(user:String, userReports:List[UserReport]) extends Event
case class ExcelMarketListUpdate(values:List[String]) extends Event
case class ExcelMarketDataUpdate(name:String, version:Int) extends Event
case class UserSettingUpdated(key:Key[_]) extends Event

object PricingGroupMarketDataUpdate {
  def matching(pricingGroupOption : Option[PricingGroup]) : PartialFunction[Event, PricingGroupMarketDataUpdate] = {
    pricingGroupOption.map(pricingGroup => matching(pricingGroup))
  }

  def matching(pricingGroup : PricingGroup) : PartialFunction[Event, PricingGroupMarketDataUpdate] = {
    case update@PricingGroupMarketDataUpdate(`pricingGroup`, _) => update
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

case class UserLoggedIn(user:User) extends Event

case class RabbitMessage(body:String, headers:Map[String,Object])

abstract class RabbitEvent(val queueName : String) extends Event {
  import sjson.json._

  def toMessage = RabbitMessage(toJSON, toMap("userName", "subGroupName"))
  def toJSON = new String(Serializer.SJSON.out(toMap()))
  def toMap(keys : String*) : Map[String, Object] = toMap.slice(keys : _*)
  def toMap : Map[String, Object]

  protected def priceEventMap(user : User, label : String, observationDate : Option[Day], dates : Array[Day]) = Map(
    "userName" -> user.username,
    "label" -> label,
    "observationDate" -> observationDate.map(_.toString("dd MMM yyyy")).getOrElse(""),
    "dates" -> dates.map(_.toString("dd MMM yyyy"))
  )
}

case class BlotterTradeUpdate(user : User, subGroupName : String, data : List[List[String]])
  extends RabbitEvent("Trafigura.Raw.Trade.RiskManagement") {

  def toMap = Map("userName" -> user.username, "subGroupName" -> subGroupName, "data" -> data)
}

case class UploadPricesUpdate(user : User, label : String, observationPoint : ObservationPoint, dates : Array[Day],
                              marketName : String, prices : Array[Double])
  extends RabbitEvent("Trafigura.Raw.Price.RiskManagement") {

  def toMap = priceEventMap(user, label, observationPoint.day, dates) ++
    Map("marketName" -> marketName, "prices" -> prices.map(_.toString))
}

case class UploadStandardDeviationsUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Day],
                                          marketName : String, standardDeviations : Array[Array[Double]])
  extends RabbitEvent("Trafigura.Raw.StandardDeviations.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("marketName" -> marketName, "standardDeviations" -> standardDeviations.map(_.map(_.toString)))
}

case class UploadVolsUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Day],
                            marketName : String, vols : Array[Array[Double]])
  extends RabbitEvent("Trafigura.Raw.Vols.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("marketName" -> marketName, "vols" -> vols.map(_.map(_.toString)))
}

case class UploadInterestRatesUpdate(user : User, label : String, observationDate : Option[Day], dates : Array[Day],
                                     currency : String, interestRates : Array[Double])
  extends RabbitEvent("Trafigura.Raw.InterestRates.RiskManagement") {

  def toMap = priceEventMap(user, label, observationDate, dates) ++
    Map("currency" -> currency, "interestRates" -> interestRates.map(_.toString))
}

case class EmailEvent(from: String = "", to: Seq[String] = Nil, subject: String = "", body: String = "") extends Event