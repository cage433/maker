package starling.metals

import starling.services.rpc.refdata.{FileMockedTitanServices, DefaultTitanServices}
import starling.services.rpc.logistics.{FileMockedTitanLogisticsServices, DefaultTitanLogisticsServices}
import starling.services.rpc.valuation._
import starling.services.rpc.marketdata.MarketDataService
import starling.tradestore.TradeStores
import starling.http.HttpServer
import starling.services.jmx.StarlingJMX
import java.util.concurrent.ConcurrentHashMap
import java.util.UUID
import starling.utils.ImplicitConversions._
import starling.auth.User
import starling.services.trinity.{XRTGenerator, FCLGenerator, TrinityUploader}
import com.trafigura.services.trinity.TrinityService
import com.trafigura.services.ResteasyServiceApi._
import com.trafigura.services.{ResteasyServiceApi, WebServiceFactory}
import starling.utils.ObservingBroadcaster._
import starling.databases.utils.{RabbitMessageSender, RabbitBroadcaster}
import starling.services.{EmailBroadcaster, Scheduler}
import starling.services.rabbit.TitanRabbitIdBroadcaster._
import starling.services.rabbit.{TitanRabbitIdBroadcaster, MockTitanRabbitEventServices, DefaultTitanRabbitEventServices}
import org.springframework.mail.javamail.JavaMailSenderImpl
import com.trafigura.services.valuation.ValuationServiceApi
import starling.manager.{ExportTitanRMIProperty, BromptonContext, BromptonActivator}
import com.trafigura.services.marketdata.{MarketDataServiceApi, ExampleService}
import starling.tradeimport.TradeImporter
import starling.richdb.{RichResultSetRowFactory, RichDB}
import starling.db.{DB, TitanTradeSystem, MarketDataStore}
import starling.calendar.BusinessCalendars
import starling.rmi.{RabbitEventDatabase, DefaultRabbitEventDatabase}
import swing.event.Event
import starling.utils._
import starling.curves.ClosesEnvironmentRule._
import starling.daterange.ObservationPoint._
import starling.daterange.{ObservationPoint, TimeOfDay}
import starling.curves._
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}
import starling.marketdata.ReferenceDataLookup
import starling.titan.TitanTradeStoreManager._
import starling.titan.{TitanTradeStoreManager, TitanSystemOfRecord, TitanTradeStore}

class MetalsBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {

    val props = context.awaitService(classOf[starling.props.Props])
    val osgiBroadcaster = context.awaitService(classOf[Broadcaster])

    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val tradeStores = context.awaitService(classOf[TradeStores])
    val referenceDataLookup = context.awaitService(classOf[ReferenceDataLookup])

    val titanTradeStore = tradeStores.titanTradeStore.asInstanceOf[TitanTradeStore]

    val starlingDB = DB(props.StarlingDatabase())

    val startRabbit = true
    val testMode = false

    val mailSender = new JavaMailSenderImpl().update(_.setHost(props.SmtpServerHost()), _.setPort(props.SmtpServerPort()))

    val titanRabbitEventServices = if (!testMode) {
      new DefaultTitanRabbitEventServices(props)
    }
    else {
      new MockTitanRabbitEventServices()
    }

    if (props.RabbitEnabled()) {
      titanRabbitEventServices.start
    }

    val (titanServices, logisticsServices) = if (!testMode) {
      (
        new DefaultTitanServices(props),
        new DefaultTitanLogisticsServices(props)
        )
    }
    else {
      (
        new FileMockedTitanServices(),
        new FileMockedTitanLogisticsServices()
        )
    }
    val titanTradeStoreManager: TitanTradeStoreManager = TitanTradeStoreManager(titanServices, titanTradeStore, titanServices, logisticsServices)

    val titanSystemOfRecord = new TitanSystemOfRecord(titanTradeStoreManager)
    val titanTradeImporter = new TradeImporter(TitanTradeSystem, titanSystemOfRecord, titanTradeStore)

    context.registerService(classOf[TradeImporter], titanTradeImporter)

    val broadcaster = ObservingBroadcaster(new CompositeBroadcaster(
        props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
        props.EnableVerificationEmails()          → new EmailBroadcaster(mailSender),
        (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher)
    ))

    context.registerService(classOf[Receiver], new Receiver() {
      def event(event: Event) = broadcaster.broadcast(event)
    })

    val trinityService = new TrinityService(ResteasyServiceApi(props.TrinityServiceUrl()))

    val rabbitEventDatabase = new DefaultRabbitEventDatabase(starlingDB, osgiBroadcaster)

    val environmentProvider = new DefaultEnvironmentProvider(marketDataStore, referenceDataLookup)
    val valuationService = new ValuationService(
      environmentProvider,
      titanTradeStore)
    val marketDataService = new MarketDataService(marketDataStore, environmentProvider)

    val eventHandler = new TitanEventHandler(
      titanRabbitEventServices,
      titanTradeStoreManager,
      environmentProvider,
      rabbitEventDatabase)

    titanRabbitEventServices.addClient(eventHandler)


    val businessCalendars = context.awaitService(classOf[BusinessCalendars])
    val curveViewer = context.awaitService(classOf[CurveViewer])

    val trinityUploader = new TrinityUploader(new FCLGenerator(businessCalendars, curveViewer),
      new XRTGenerator(marketDataStore), trinityService, props)

    val scheduler = Scheduler.create(businessCalendars, marketDataStore, broadcaster, trinityUploader, props)
    scheduler.start

    new StarlingJMX(scheduler).start

    val serverName = props.ServerName()

    lazy val webServiceServer = locally {
      val webXmlUrl = "services/resources/webapp/WEB-INF/web.xml"//classOf[StarlingInit].getResource("../../webapp/WEB-INF/web.xml").toExternalForm

      new HttpServer(props.HttpServicePort(), props.HttpServiceExternalUrl(), serverName, Some(webXmlUrl), Nil) {
        override def start =
          log.infoF("HTTP web service external url = '%s', server name = '%s'" % (props.HttpServiceExternalUrl(), serverName)) {
            super.start; /*DocumentationService.registerInstances(webServices : _*) */
          }
      }
    }

    StarlingWebServices.webServices = List(marketDataService, valuationService, /*DocumentationService, */ExampleService)

    context.registerService(classOf[ValuationServiceApi], valuationService, ExportTitanRMIProperty::Nil)
    context.registerService(classOf[MarketDataServiceApi], marketDataService,ExportTitanRMIProperty::Nil)

    context.registerService(classOf[RabbitEventDatabase], rabbitEventDatabase)

    {
      import starling.daterange.ObservationTimeOfDay._

      val metalRules = List(
        ClosesEnvironmentRule(referenceDataLookup),
        ClosesEnvironmentRule(referenceDataLookup, allowOldPricesToBeUsed = true),
        new VanillaEnvironmentRule(_.atTimeOfDay(SHFEClose), TimeOfDay.EndOfDay, new EnvironmentRuleLabel(SHFEClose.name),
          List(PricingGroup.Metals), referenceDataLookup),
        new TimeShiftToLMECloseEnvironmentRule(referenceDataLookup)
      )

      metalRules.foreach(context.registerService(classOf[EnvironmentRule], _))


    }

  }

  def stop(context: BromptonContext) {
  }
}

class StarlingWebServices extends WebServiceFactory {
  override val services = {
    if (StarlingWebServices.webServices == null) {
      throw new Exception("StarlingWebServices.services has not been initialised")
    } else {
      StarlingWebServices.webServices
    }
  }
}

object StarlingWebServices {
  var webServices:List[AnyRef] = _
}
