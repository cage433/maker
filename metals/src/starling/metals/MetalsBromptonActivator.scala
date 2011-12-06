package starling.metals

import datasources._
import swing.event.Event

import starling.curves._
import starling.databases.utils.{RabbitMessageSender, RabbitBroadcaster}
import starling.daterange.TimeOfDay
import starling.manager._
import starling.marketdata.{MarketDataTypes, ReferenceDataLookup}
import starling.services.jmx.StarlingJMX
import starling.services.rabbit.{TitanRabbitIdBroadcaster, MockTitanRabbitEventServices, DefaultTitanRabbitEventServices}
import starling.services.rpc.logistics.{FileMockedTitanLogisticsServices, DefaultTitanLogisticsServices}
import starling.services.rpc.valuation._
import starling.metals.trinity.XRTGenerator
import starling.services.trinity.{FCLGenerator}
import starling.titan.{TitanTradeStoreManager, TitanSystemOfRecord, TitanTradeStore}
import starling.tradestore.TradeStores
import starling.utils._
import starling.utils.ImplicitConversions._
import starling.scheduler.ScheduledTime._
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.ResteasyServiceApi
import org.joda.time.Period
import starling.scheduler.{TaskDescription, Scheduler}
import starling.services._
import rpc.refdata.LogisticsService
import starling.db._
import starling.lim.LIMService
import starling.gui.api.Email._
import starling.gui.api.{Email, MarketDataSelection, PricingGroup}
import starling.metals.tasks.{UploadCurveToTrinityTask, TrinityUploader}
import starling.props.ServerTypeLabel._
import starling.calendar.{BusinessCalendar, BusinessCalendars}
import com.trafigura.services.Logistics.LogisticsServiceApi

class MetalsBromptonActivator extends BromptonActivator with Log with scalaz.Identitys {
  def start(context: BromptonContext) {

    val props = context.awaitService(classOf[starling.props.Props])
    val startRabbit = true
    val testMode = false

    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val referenceDataLookup = context.awaitService(classOf[ReferenceDataLookup])

    val titanTradeStore = {
      val tradeStores = context.awaitService(classOf[TradeStores])

      tradeStores.titanTradeStore.asInstanceOf[TitanTradeStore]
    }

    val titanRabbitEventServices = if (!testMode) {
      new DefaultTitanRabbitEventServices(props)
    }
    else {
      new MockTitanRabbitEventServices()
    }

    if (props.RabbitEnabled()) {
      titanRabbitEventServices.start
    }

    val osgiBroadcaster = context.awaitService(classOf[Broadcaster])

    val broadcaster = new CompositeBroadcaster(
      props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
      (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher)
    )

    context.registerService(classOf[Receiver], new Receiver() {
      def event(event: Event) = broadcaster.broadcast(event)
    })

    val environmentProvider = new DefaultEnvironmentProvider(marketDataStore, referenceDataLookup)

    import starling.services.rpc.refdata.{FileMockedTitanServices, DefaultTitanServices}

    val (titanServices, logisticsServices) = if (!testMode) (
      new DefaultTitanServices(props),
      new DefaultTitanLogisticsServices(props)
    )
    else (
      new FileMockedTitanServices(),
      new FileMockedTitanLogisticsServices()
    )

    val titanTradeStoreManager: TitanTradeStoreManager = {
      val manager = TitanTradeStoreManager(titanServices, titanTradeStore, titanServices, logisticsServices)
      context.onStarted { manager.start }
      manager
    }

    val valuationSnapshotCreator = new MetalValuationSnapshotCreator(osgiBroadcaster, environmentProvider, titanTradeStore)
    context.registerService(classOf[Receiver], valuationSnapshotCreator)

    {
      import starling.rmi.{RabbitEventDatabase, DefaultRabbitEventDatabase}

      val starlingDB = DB(props.StarlingDatabase( ))
      val rabbitEventDatabase = new DefaultRabbitEventDatabase(starlingDB, osgiBroadcaster)

      titanRabbitEventServices.addClient(new TitanEventHandler(osgiBroadcaster, titanTradeStoreManager,
        environmentProvider, rabbitEventDatabase))

      context.registerService(classOf[RabbitEventDatabase], rabbitEventDatabase)
    }

    {
      import starling.tradeimport.TradeImporter

      val tradeImporter = new TradeImporter(TitanTradeSystem, new TitanSystemOfRecord(titanTradeStoreManager), titanTradeStore)

      context.registerService(classOf[TradeImporter], tradeImporter)
    }

    val bloombergImports = DBBloombergImports.importsFrom(DB(props.EAIReplica()))

    {
      val limService = LIMService(props.LIMHost(), props.LIMPort())
      val template = Email(props.MetalsEmailAddress(), props.LimEmailAddress())
      val emailService = context.awaitService(classOf[EmailService]).enabledIf(props.EnableVerificationEmails())

      val marketDataSourcesFactories = List(SpotFXLimMarketDataSource.apply _, PriceFixingLimMarketDataSource.apply _,
        ForwardRateLimMarketDataSource.apply _, PriceLimMarketDataSource.apply(bloombergImports) _)

      marketDataSourcesFactories.foreach(factory =>
        context.registerService(classOf[MarketDataSource], factory(limService, emailService, template)))
    }

    val dataTypes = new MarketDataTypes(referenceDataLookup)

    registerScheduler(context, osgiBroadcaster, dataTypes)

    Map(
      "Bloomberg → LIM"    → new BloombergImportsReferenceData(bloombergImports),
      "Areas"              → NeptuneReferenceData.areas(referenceDataLookup),
      "Contract Locations" → NeptuneReferenceData.contractLocations(referenceDataLookup),
      "Countries"          → NeptuneReferenceData.countries(referenceDataLookup),
      "Grades"             → NeptuneReferenceData.grades(referenceDataLookup),
      "Incoterms"          → NeptuneReferenceData.incoterms(referenceDataLookup)
    ).map(ReferenceData.tupled).map(context.registerService(classOf[ReferenceData], _))

    {
      import com.trafigura.services.valuation.ValuationServiceApi

      val valuationService = new ValuationService(environmentProvider, titanTradeStore)

      context.registerService(classOf[ValuationServiceApi], valuationService, ServiceProperties(ExportTitanRMIProperty, ExportTitanHTTPProperty))
    }

    {
      import com.trafigura.services.marketdata._
      import starling.services.rpc.marketdata._
      val marketDataService = new MarketDataService(marketDataStore)
      val logisticsService = new LogisticsService(titanServices, logisticsServices)

      context.registerService(classOf[MarketDataServiceApi], marketDataService, ServiceProperties(ExportTitanRMIProperty, ExportTitanHTTPProperty))
      context.registerService(classOf[ExampleServiceApi], ExampleService, ServiceProperties(ExportTitanHTTPProperty))

      context.registerService(classOf[LogisticsServiceApi], logisticsService, ServiceProperties(ExportTitanHTTPProperty))
    }

    {
      import starling.daterange.ObservationTimeOfDay._
      import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}

      val metalRules = List(
        ClosesEnvironmentRule(referenceDataLookup),
        new VanillaEnvironmentRule(_.atTimeOfDay(SHFEClose), TimeOfDay.EndOfDay, new EnvironmentRuleLabel(SHFEClose.name),
          List(PricingGroup.Metals), referenceDataLookup, dataTypes),
        new TimeShiftToLMECloseEnvironmentRule(referenceDataLookup, dataTypes)
      )

      metalRules.foreach(context.registerService(classOf[EnvironmentRule], _))
    }
  }

  // TODO [19 Oct 2011] Reuse scheduler for oil tasks
  private def registerScheduler(context: BromptonContext, broadcaster: Broadcaster, dataTypes: MarketDataTypes) {
    val props = context.awaitService(classOf[starling.props.Props])
    val scheduler = new Scheduler(props)
    val jmx = new StarlingJMX(scheduler)

    context.onStarted { scheduler.start; jmx.start }
    context.registerService(classOf[ReferenceData], ReferenceData("Schedules", new SchedulerReferenceData(scheduler)))
    context.createServiceTracker(Some(classOf[TaskDescription]), tracker = new BromptonServiceCallback[TaskDescription] {
      def serviceAdded(ref: BromptonServiceReference, properties: ServiceProperties, task: TaskDescription) = {
        scheduler += task
      }
    })

    if (props.ServerType() == FC2 && props.ImportMarketDataAutomatically()) registerFC2Tasks(context, broadcaster, dataTypes)
  }

  private def registerFC2Tasks(context: BromptonContext, broadcaster: Broadcaster, dataTypes: MarketDataTypes) {
    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val businessCalendars = context.awaitService(classOf[BusinessCalendars])

    context.registerService(classOf[TaskDescription], importLim(businessCalendars, marketDataStore))

    registerDataValidationTasks(context, broadcaster)
    registerTrinityTask(context)
  }

  private def importLim(businessCalendars: BusinessCalendars, marketDataStore: MarketDataStore) = TaskDescription("Import LIM",
    everyFiveMinutes(businessCalendars.everyDay()), new ImportMarketDataTask(marketDataStore, PricingGroup.Metals)
      .withSource("LIM", marketDataStore.sourcesFor(PricingGroup.Metals).flatMap(_.description): _*), coolDown = Period.minutes(3))

  private def registerDataValidationTasks(context: BromptonContext, broadcaster: Broadcaster) {
    val marketDataStore = context.awaitService(classOf[MarketDataStore])

    val marketDataSources = marketDataStore.sourcesFor(PricingGroup.Metals)

    val marketDataAvailabilityBroadcaster = new MarketDataAvailabilityBroadcaster(marketDataStore, broadcaster,
      marketDataSources.flatMap(_.eventSources(marketDataStore)))

    marketDataSources.flatMap(_.availabilityTasks(marketDataStore)).foreach(context.registerService(classOf[TaskDescription], _))

    context.registerService(classOf[Receiver], marketDataAvailabilityBroadcaster)
  }

  private def registerTrinityTask(context: BromptonContext) {
    val props = context.awaitService(classOf[starling.props.Props])
    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val businessCalendars = context.awaitService(classOf[BusinessCalendars])

    val curveViewer = context.awaitService(classOf[CurveViewer])
    val trinityService = new TrinityService(ResteasyServiceApi(props.TrinityServiceUrl()))
    val trinityUploader = new TrinityUploader(new FCLGenerator(businessCalendars, curveViewer),
          new XRTGenerator(marketDataStore), trinityService, props)

    val task = TaskDescription("Upload Curves to Trinity", daily(businessCalendars.LME, 19 H 00),
      new UploadCurveToTrinityTask(trinityUploader,
        marketDataStore.latestMarketDataIdentifier(MarketDataSelection(Some(PricingGroup.Metals)))))

    context.registerService(classOf[TaskDescription], task)

    //val uploadLibor = SimpleScheduledTask(trinityUploader.uploadLibor).withSink("Trinity")
    //TaskDescription("Upload Libor to Trinity", daily(businessCalendars.LME, 23 H 45), uploadLibor)
  }
}
