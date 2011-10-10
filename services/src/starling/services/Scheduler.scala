package starling.services

import java.util.Timer

import starling.calendar.BusinessCalendars
import starling.db.MarketDataStore
import starling.gui.api.{PricingGroup, MarketDataSelection}
import starling.props.Props
import starling.services.trinity.TrinityUploader
import starling.utils.{ObservingBroadcaster, Log, Stopable}

import starling.market.FuturesExchangeFactory._
import starling.utils.ImplicitConversions._


class Scheduler(props: Props, forwardCurveTasks: List[TaskDescription] = Nil) extends Stopable with Log {
  private lazy val timer = new Timer(true)
  val tasks = if (props.ServerType() == "FC2") forwardCurveTasks else Nil

  override def start = log.infoF("%s tasks for ServerType: %s" % (tasks.size, props.ServerType())) {
    super.start; tasks.map(_.schedule(timer))
  }

  override def stop = { super.stop; timer.cancel }
}

object Scheduler extends Log {
  import PricingGroup._
  import ScheduledTime._

  def create(businessCalendars: BusinessCalendars, marketDataStore: MarketDataStore, observingBroadcaster: ObservingBroadcaster,
             trinityUploader: TrinityUploader, props: Props): Scheduler = log.infoWithTime("Creating scheduler") {

    val broadcaster = observingBroadcaster.broadcaster

    val limFlows@List(lmeMetals, sfsMetals, comexMetals): List[DataFlow with MarketDataEventSource] = List(LME, SFS, COMEX)
      .map(new DataFlow(_, Metals, Nil, props.MetalsEmailAddress(), props.LimEmailAddress(), "LIM") with NullMarketDataEventSource)

    val balticMetals = new DataFlow(BALTIC, Metals, List("Panamax T/C Average (Baltic)"), props.MetalsEmailAddress(), props.WuXiEmailAddress(), "Excel") with NullMarketDataEventSource
    val exbxgMetals = new DataFlow(EXBXG, Metals, Nil, props.MetalsEmailAddress(), props.WuXiEmailAddress(), "Excel") with NullMarketDataEventSource
    val spotfx = SpotFXDataEventSource(Metals, SpotFXDataProvider(marketDataStore))
    val libor = new PriceFixingDataEventSource(Metals, ReferenceInterestDataProvider(marketDataStore))

    val marketDataAvailabilityBroadcaster = new MarketDataAvailabilityBroadcaster(observingBroadcaster,
      exbxgMetals :: spotfx :: libor :: limFlows)

    implicit def enrichDataFlow(dataFlow: DataFlow with MarketDataEventSource) = new {
      val verifyPricesAvailable = ("Verify %s available" % dataFlow.sink) → VerifyPriceAvailable(marketDataAvailabilityBroadcaster, broadcaster, dataFlow)
      val verifyPricesValid = ("Verify %s valid" % dataFlow.sink) → VerifyPricesValid(marketDataStore, broadcaster, dataFlow)
    }

    def importMarketData(pricingGroup: PricingGroup) = new ImportMarketDataTask(marketDataStore, pricingGroup).
      withSource("LIM", marketDataStore.sourcesFor(pricingGroup).flatMap(_.description) : _*)

    def uploadCurvesToTrinity(pricingGroup: PricingGroup) =
      new UploadCurveToTrinityTask(trinityUploader, marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(pricingGroup))))

    val verifyLiborMaturities = new VerifyLiborMaturitiesAvailable(marketDataStore, broadcaster, props.MetalsEmailAddress(),
      props.LimEmailAddress()).withSource("LIM")

    val uploadLibor = SimpleScheduledTask(trinityUploader.uploadLibor).withSink("Trinity")

    def tasks(time: ScheduledTime, tasks: (String, ScheduledTask)*) =
      tasks.toList.map(nameTask => TaskDescription(nameTask._1, time, nameTask._2))

    log.info("instantiating...")

    new Scheduler(props, forwardCurveTasks =
      TaskDescription("Import LIM", everyFiveMinutes(businessCalendars.LME), importMarketData(Metals)) ::-
      tasks(daily(businessCalendars.LME, 18 H 30), balticMetals.verifyPricesAvailable) ::-
      TaskDescription("Upload Curves to Trinity", daily(businessCalendars.LME, 19 H 00), uploadCurvesToTrinity(Metals)) ::-
      tasks(daily(businessCalendars.SFE, 16 H 30), exbxgMetals.verifyPricesAvailable, exbxgMetals.verifyPricesValid) ::-
      tasks(daily(businessCalendars.LME, 23 H 30),
        lmeMetals.verifyPricesAvailable, sfsMetals.verifyPricesAvailable, comexMetals.verifyPricesAvailable,
        lmeMetals.verifyPricesValid,     sfsMetals.verifyPricesValid,     comexMetals.verifyPricesValid
      ) ::-
      TaskDescription("Verify Libor maturities available", daily(businessCalendars.LME, 13 H 00), verifyLiborMaturities) //::-
      //TaskDescription("Upload Libor to Trinity", daily(businessCalendars.LME, 23 H 45), uploadLibor)
    )
  }
}