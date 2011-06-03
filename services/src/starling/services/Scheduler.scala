package starling.services

import java.util.Timer

import starling.calendar.BusinessCalendars
import starling.db.MarketDataStore
import starling.gui.api._
import starling.market.{FuturesExchange, FuturesExchangeFactory}
import starling.props.Props
import starling.services.trinity.TrinityUploader
import starling.utils.{Broadcaster, Log, Stoppable}

import starling.utils.ImplicitConversions._


class Scheduler(props: Props, forwardCurveTasks: List[TaskDescription] = Nil) extends Stoppable {
  private lazy val timer = new Timer(true)
  val tasks = if (props.ServerType() == "FC2") forwardCurveTasks else Nil

  def start = Log.infoF("Scheduling %s tasks for ServerType: %s" % (tasks.size, props.ServerType())) {
    tasks.map { case task => task.schedule(timer) }
  }

  def stop = timer.cancel
}

object Scheduler {
  import PricingGroup._
  import FuturesExchangeFactory._
  import ScheduledTime._

  def create(businessCalendars: BusinessCalendars, marketDataStore: MarketDataStore, broadcaster: Broadcaster,
             trinityUploader: TrinityUploader, props: Props): Scheduler = {

    def verifyPricesAvailable(pricingGroup: PricingGroup, exchange: FuturesExchange, to: String) =
      new VerifyPriceAvailable(marketDataStore, pricingGroup, exchange, broadcaster, props.MetalsEmailAddress(), to)

    def verifyPricesValid(pricingGroup: PricingGroup, exchange: FuturesExchange, to: String) =
      VerifyPricesValid(marketDataStore, pricingGroup, exchange, broadcaster, props.MetalsEmailAddress(), props.WuXiEmailAddress())

    // TODO [19 May 2011, Stacy]: Consider breaking this up so different sources of data can be shown in the Reference Data page
    def importMarketData(pricingGroup: PricingGroup) = new ImportMarketDataTask(marketDataStore, pricingGroup).withSource("Starling")

    def uploadCurvesToTrinity(pricingGroup: PricingGroup) =
      new UploadCurveToTrinityTask(trinityUploader, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(pricingGroup))))

    val verifyLiborMaturities = new VerifyLiborMaturitiesAvailable(marketDataStore, broadcaster, props.MetalsEmailAddress(),
      props.LimEmailAddress()).withSource("LIM")

    val uploadLibor = SimpleScheduledTask(trinityUploader.uploadLibor).withSink("Trinity")

    def tasks(time: ScheduledTime, tasks: (String, ScheduledTask)*) =
      tasks.toList.map(nameTask => TaskDescription(nameTask._1, time, nameTask._2))

    new Scheduler(props, forwardCurveTasks =
      TaskDescription("Import LIM", hourly(businessCalendars.LME), importMarketData(Metals)) ::-
      //TaskDescription("Upload Curves to Trinity", daily(businessCalendars.LME, 12 H 30), uploadCurvesToTrinity(Metals)) ::-
      tasks(daily(businessCalendars.SFE, 16 H 30),
        "Verify WuXi prices available" → verifyPricesAvailable(Metals, EXBXG, props.WuXiEmailAddress()).withSource("Excel"),
        "Verify WuXi prices valid"     → verifyPricesValid(Metals, EXBXG, props.MetalsEmailAddress()).withSource("Excel")
      ) ::-
      tasks(daily(businessCalendars.LME, 23 H 30),
        "Verify LME LIM Metals available"   → verifyPricesAvailable(Metals, LME,   props.LimEmailAddress()).withSource("LIM"),
        "Verify SHFE LIM Metals available"  → verifyPricesAvailable(Metals, SFS,   props.LimEmailAddress()).withSource("LIM"),
        "Verify COMEX LIM Metals available" → verifyPricesAvailable(Metals, COMEX, props.LimEmailAddress()).withSource("LIM"),
        "Verify LME LIM Metals valid"       → verifyPricesValid(Metals, LME,   props.LimEmailAddress()).withSource("LIM"),
        "Verify SHFE LIM Metals valid"      → verifyPricesValid(Metals, SFS,   props.LimEmailAddress()).withSource("LIM"),
        "Verify COMEX LIM Metals valid"     → verifyPricesValid(Metals, COMEX, props.LimEmailAddress()).withSource("LIM")
      ) ::-
      TaskDescription("Verify Libor maturities available", daily(businessCalendars.LME, 23 H 30), verifyLiborMaturities) //::-
      //TaskDescription("Upload Libor to Trinity", daily(businessCalendars.LME, 23 H 45), uploadLibor)
    )
  }
}