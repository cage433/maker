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
import starling.titan.{TitanSystemOfRecord, TitanTradeStore, TitanTradeCache}
import starling.tradeimport.TradeImporter
import starling.utils.{Broadcaster, ObservingBroadcaster, CompositeBroadcaster}
import starling.richdb.{RichResultSetRowFactory, RichDB}
import starling.db.{DB, TitanTradeSystem, MarketDataStore}
import starling.calendar.BusinessCalendars
import starling.curves.CurveViewer
import starling.rmi.{RabbitEventDatabase, DefaultRabbitEventDatabase}


class MetalsBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {

    val props = context.awaitService(classOf[starling.props.Props])
    val osgiBroadcaster = context.awaitService(classOf[Broadcaster])

    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val tradeStores = context.awaitService(classOf[TradeStores])

    val titanTradeStore = tradeStores.titanTradeStore

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

    val (titanTradeCache : TitanTradeCache, titanServices, logisticsServices, titanInventoryCache) = if (!testMode) {
        val titanTradeCache = new DefaultTitanTradeCache(props)
        val titanServices = new DefaultTitanServices(props)
        val titanLogisticsServices = new DefaultTitanLogisticsServices(props)
        (
          titanTradeCache,
          titanServices,
          titanLogisticsServices,
          new DefaultTitanLogisticsInventoryCache(
            titanLogisticsServices,
            titanTradeCache,
            titanServices,
            Some(titanTradeStore))
        )
      }
      else {
        val fileMockedTitanServices = new FileMockedTitanServices()
        val fileMockedTitanLogisticsServices = new FileMockedTitanLogisticsServices()
        val mockTitanTradeService = new DefaultTitanTradeService(fileMockedTitanServices)
        (
          new TitanTradeServiceBasedTradeCache(mockTitanTradeService),
          fileMockedTitanServices,
          fileMockedTitanLogisticsServices,
          new DefaultTitanLogisticsInventoryCache(fileMockedTitanLogisticsServices, null/*titanTradeCache*/, fileMockedTitanServices, Some(titanTradeStore))
        )
      }

    val titanSystemOfRecord = new TitanSystemOfRecord(titanTradeCache, titanServices, logisticsServices)
    val titanTradeImporter = new TradeImporter(TitanTradeSystem, titanSystemOfRecord, titanTradeStore)

    context.registerService(classOf[TradeImporter], titanTradeImporter)

    val broadcaster = ObservingBroadcaster(new CompositeBroadcaster(
        props.rabbitHostSet                       → new RabbitBroadcaster(new RabbitMessageSender(props.RabbitHost())),
        props.EnableVerificationEmails()          → new EmailBroadcaster(mailSender),
        (startRabbit && props.titanRabbitHostSet) → TitanRabbitIdBroadcaster(titanRabbitEventServices.rabbitEventPublisher),
        true                                      → osgiBroadcaster
    ))

    val trinityService = new TrinityService(ResteasyServiceApi(props.TrinityServiceUrl()))

    val rabbitEventDatabase = new DefaultRabbitEventDatabase(starlingDB, broadcaster)

    val valuationService = new ValuationService(
      new DefaultEnvironmentProvider(marketDataStore),
      titanTradeCache, titanServices, logisticsServices, titanRabbitEventServices,
      titanInventoryCache, Some(titanTradeStore), rabbitEventDatabase)
    val marketDataService = new MarketDataService(marketDataStore, new DefaultEnvironmentProvider(marketDataStore))

    val businessCalendars = context.awaitService(classOf[BusinessCalendars])
    val curveViewer = context.awaitService(classOf[CurveViewer])

    val trinityUploader = new TrinityUploader(new FCLGenerator(businessCalendars, curveViewer),
      new XRTGenerator(marketDataStore), trinityService, props)

    val scheduler = Scheduler.create(businessCalendars, marketDataStore, broadcaster, trinityUploader, props)

    val jmx = new StarlingJMX(scheduler)

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
