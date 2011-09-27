package starling.services.osgi

import starling.props.PropsHelper
import starling.services.{StarlingInit}
import starling.auth.AuthHandler
import com.trafigura.services.valuation.ValuationServiceApi
import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.fc2.api.FC2Service
import starling.browser.service.BrowserService
import starling.manager._
import starling.utils.{Broadcaster}
import starling.db.{MarketDataStore, DB}
import starling.tradestore.TradeStores
import starling.calendar.BusinessCalendars
import starling.rmi.{RabbitEventDatabase, UserSettingsDatabase, BromptonTrackerBasedMarketDataPageIdentifierReaderProviders, StarlingServer}
import starling.curves.CurveViewer

class ServicesProps
class ServicesBromptonActivator extends BromptonActivator {

  type Props = ServicesProps
  def defaults = new ServicesProps

  var starlingInit:StarlingInit = _

  def start(context: BromptonContext) {
    val authHandler = context.awaitService(classOf[AuthHandler])
    val osgiBroadcaster = context.awaitService(classOf[Broadcaster])

    val props = context.awaitService(classOf[starling.props.Props])

    val bromptonMarketDataReaderProviders = new BromptonTrackerBasedMarketDataPageIdentifierReaderProviders(context)

    starlingInit = new StarlingInit(
      props,
      authHandler, osgiBroadcaster,
      true, true, true,
      forceGUICompatability=false,
      startEAIAutoImportThread=props.ImportsBookClosesFromEAI(),
      startRabbit = props.RabbitEnabled(),
      marketDataReadersProviders = bromptonMarketDataReaderProviders
    )
    context.registerService(classOf[StarlingServer], starlingInit.starlingServer,ExportGuiRMIProperty::Nil)
    context.registerService(classOf[FC2Service], starlingInit.fc2Service,ExportGuiRMIProperty::Nil)
    context.registerService(classOf[BrowserService], starlingInit.browserService,ExportGuiRMIProperty::Nil)


    context.registerService(classOf[UserSettingsDatabase], starlingInit.userSettingsDatabase)
    context.registerService(classOf[MarketDataStore], starlingInit.marketDataStore)
    context.registerService(classOf[BusinessCalendars], starlingInit.businessCalendars)
    context.registerService(classOf[CurveViewer], starlingInit.curveViewer)

    starlingInit.loopyXLReceivers.foreach { receiver => {
      context.registerService(classOf[AnyRef], receiver, ExportExcelProperty::Nil)
    }}

    starlingInit.start
    //starlingInit.servlets.foreach { case (name, servlet) => context.registerService(classOf[HttpServlet], servlet, List(HttpContext(name)))}
  }

  def init(context: BromptonContext, props: ServicesProps) { }

  def stop(context: BromptonContext) {
    if (starlingInit != null) {
      starlingInit.stop
    }
  }
}