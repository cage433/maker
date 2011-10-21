package starling.services

import java.util.Timer

import starling.props.Props
import starling.utils.{Log, Stopable}

import starling.utils.ImplicitConversions._
import collection.mutable.ListBuffer


class Scheduler(props: Props, initialTasks: List[TaskDescription] = Nil) extends Stopable with Log {
  def getTasks: List[TaskDescription] = tasks.toList

  private val tasks = new ListBuffer[TaskDescription]() ++= initialTasks
  private lazy val timer = new Timer(true)

  override def start = log.infoF("%s tasks for ServerType: %s" %(tasks.size, props.ServerType())) {
    super.start;
    tasks.map(_.schedule(timer))
  }

  override def stop = {
    super.stop; timer.cancel
  }
}

object Scheduler extends Log {

  import PricingGroup._
  import ScheduledTime._

  def create(businessCalendars: BusinessCalendars, fc2FacilityImpl:FC2FacilityImpl, marketDataStore: MarketDataStore, observingBroadcaster: ObservingBroadcaster,
             trinityUploader: TrinityUploader, props: Props): Scheduler = log.infoWithTime("Creating scheduler") {
    if (props.ServerType() == "FC2" && props.ImportMarketDataAutomatically()) {
      val broadcaster = observingBroadcaster.broadcaster

      val limFlows@List(lmeMetals, sfsMetals, comexMetals): List[DataFlow with MarketDataEventSource] = List(LME, SFS, COMEX)
        .map(new DataFlow(_, Metals, Nil, props.MetalsEmailAddress(), props.LimEmailAddress(), "LIM") with NullMarketDataEventSource)

      val balticMetals = new DataFlow(BALTIC, Metals, List("Panamax T/C Average (Baltic)"), props.MetalsEmailAddress(), props.WuXiEmailAddress(), "Excel") with NullMarketDataEventSource
      val exbxgMetals = new DataFlow(EXBXG, Metals, Nil, props.MetalsEmailAddress(), props.WuXiEmailAddress(), "Excel") with NullMarketDataEventSource
      val spotfx = SpotFXDataEventSource(Metals, SpotFXDataProvider(marketDataStore))
      val libor = new PriceFixingDataEventSource(Metals, ReferenceInterestDataProvider(marketDataStore))

      val marketDataAvailabilityBroadcaster = new MarketDataAvailabilityBroadcaster(observingBroadcaster,
        exbxgMetals :: spotfx :: libor :: limFlows)

  def +=(task: TaskDescription) = {
    tasks += task

    if (isRunning) task.schedule(timer)
  }
}
