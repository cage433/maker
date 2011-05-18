package starling.services

import java.util.{TimerTask, Timer}
import org.joda.time._
import org.joda.time.format.DateTimeFormat
import swing.event.Event

import starling.calendar.{BusinessCalendar, BusinessCalendars}
import starling.daterange.Day
import starling.db.MarketDataStore
import starling.gui.api._
import starling.market.{FuturesExchange, FuturesExchangeFactory}
import starling.pivot._
import starling.props.Props
import starling.rmi.TrinityUploader
import starling.utils.{Broadcaster, Log, Stoppable}

import starling.utils.ImplicitConversions._


class Scheduler(val tasks: List[TaskDescription]) extends Stoppable {
  private val timer = new Timer(true)

  def stop = timer.cancel
  def start = tasks.map { case task => task.schedule(timer) }
}

object Scheduler {
  import PricingGroup._
  import FuturesExchangeFactory._
  import ScheduledTime._

  def create(businessCalendars: BusinessCalendars, marketDataStore: MarketDataStore, broadcaster: Broadcaster,
             trinityUploader: TrinityUploader, props: Props): Scheduler = {

    def verifyPricesAvailable(pricingGroup: PricingGroup, exchange: FuturesExchange, to: String) = {
      new VerifyPriceAvailable(marketDataStore, pricingGroup, exchange, broadcaster, props.MetalsEmailAddress(), to)
    }

    def verifyPricesValid(pricingGroup: PricingGroup, exchange: FuturesExchange, to: String) = {
      VerifyPricesValid(marketDataStore, pricingGroup, exchange, broadcaster, props.MetalsEmailAddress(), props.WuXiEmailAddress())
    }

    def importMarketData(pricingGroup: PricingGroup) = {
      new ImportMarketDataTask(marketDataStore, pricingGroup)
    }

    def uploadToTrinity(pricingGroup: PricingGroup) = {
      new UploadCurveToTrinityTask(trinityUploader, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(pricingGroup))))
    }

    def verifyLiborMaturities = {
      new VerifyLiborMaturitiesAvailable(marketDataStore, broadcaster, props.MetalsEmailAddress(), props.LimEmailAddress())
    }

    def tasks(time: ScheduledTime, tasks: (String, ScheduledTask)*) =
      tasks.toList.map(nameTask => TaskDescription(nameTask._1, time, nameTask._2))

    new Scheduler(
      TaskDescription("Import LIM", hourly(businessCalendars.LME), importMarketData(Metals)) ::-
      tasks(daily(businessCalendars.SFE, 16 H 30),
        "Verify WuXi prices available" → verifyPricesAvailable(Metals, EXBXG, props.WuXiEmailAddress()),
        "Verify WuXi prices valid"     → verifyPricesValid(Metals, EXBXG, props.MetalsEmailAddress())
      ) ::-
      tasks(daily(businessCalendars.LME, 23 H 30),
        "Verify LME LIM Metals available"   → verifyPricesAvailable(Metals, LME, props.LimEmailAddress()),
        "Verify LME LIM Metals valid"       → verifyPricesValid(Metals, LME, props.LimEmailAddress()),
        "Verify SHFE LIM Metals available"  → verifyPricesAvailable(Metals, SFS, props.LimEmailAddress()),
        "Verify SHFE LIM Metals valid"      → verifyPricesValid(Metals, SFS, props.LimEmailAddress()),
        "Verify COMEX LIM Metals available" → verifyPricesAvailable(Metals, COMEX, props.LimEmailAddress()),
        "Verify COMEX LIM Metals valid"     → verifyPricesValid(Metals, COMEX, props.LimEmailAddress())
      ) ::-
      TaskDescription("Verify Libor maturities available", daily(businessCalendars.LME, 12 H 15), verifyLiborMaturities) ::-
      TaskDescription("Upload to Trinity", daily(businessCalendars.LME, 12 H 30), uploadToTrinity(Metals))
    )
  }
}

case class TaskDescription(name: String, time: ScheduledTime, task: ScheduledTask) extends TimerTask {
  val cal = time.cal

  def schedule(timer: Timer) =
    Log.infoF("Scheduled: %s @ %s, %s %s" % (name, time.prettyTime, time.description, time.cal.name)) {
      time.schedule(this, timer)
    }

  def run = if (Day.today.isBusinessDay(cal)) Log.infoF("Executing scheduled task: " + name) {
    task.execute(Day.today)
  } else {
    Log.info("Not a business day in calendar: %s, thus skipping: " % (cal.name, name))
  }
}

case class ScheduledTime(description: String, time: DateTime, period: Period, cal: BusinessCalendar) {
  private val periodInSeconds = period.toStandardSeconds.getSeconds.require(_ > 0, "Period cannot be 0")

  def schedule(task: TimerTask, timer: Timer) = timer.scheduleAtFixedRate(task, time.toDate, periodInSeconds * 1000)

  val prettyTime = time.toString(DateTimeFormat.forPattern("HH:mm"))
}

object ScheduledTime {
  val OneDay = Period.days(1)
  val OneHour = Period.hours(1)
  val OneMinute = Period.minutes(1)
  val Midnight = new LocalTime(0, 0)

  def everyMinute(cal: BusinessCalendar) =
    new ScheduledTime("Every minute", usingCalendar(cal, OneMinute, cal.location.now.toLocalTime), OneMinute, cal)

  def daily(cal: BusinessCalendar, time: LocalTime = 0 H 0) =
    new ScheduledTime("Every day", usingCalendar(cal, OneDay, time), OneDay, cal)

  def hourly(cal: BusinessCalendar) = new ScheduledTime("Every hour", usingCalendar(cal, OneHour, Midnight), OneHour, cal)

  private def usingCalendar(cal: BusinessCalendar, period: Period, time: LocalTime): DateTime = {
    val now = cal.location.now

    val scheduledTime = Day.today.atTimeOfDay(time, cal.location)

    if (scheduledTime < now) {
      val periodsToAdd = ((now - scheduledTime) / period.toStandardSeconds.getSeconds) + 1
      scheduledTime + (periodsToAdd * period.toStandardSeconds.getSeconds * 1000)
    } else {
      scheduledTime
    }
  }
}

trait ScheduledTask {
  def execute(observationDay: Day)

  protected def fields(names: String*) = names.map(Field(_)).toList
  protected def filters(filters: (String, Any)*): List[(Field, Selection)] =
    filters.toMap.mapKeys(Field(_)).mapValues(value => SomeSelection(Set(value))).toList
  protected def filterToString(filter: List[(Field, Selection)]) = {
    filter.toMap.mapKeys(_.name).mapValues(_.description).map("%s = %s" % _).mkString(", ")
  }
}

abstract class BroadcastingScheduledTask(broadcaster: Broadcaster) extends ScheduledTask {
  def execute(observationDay: Day) = eventFor(observationDay).map(broadcaster.broadcast)

  protected def eventFor(observationDay: Day): Option[Event]
  protected def emailFor(from: String, to: Seq[String]) = EmailEvent(from, to)
}