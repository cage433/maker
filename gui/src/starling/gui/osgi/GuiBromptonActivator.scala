package starling.gui.osgi

import starling.rmi.StarlingServer
import starling.fc2.api.FC2Facility
import starling.browser.service.BrowserService
import starling.gui.xstream.GuiStarlingXStream
import starling.browser.service.internal.HeterogeneousMap
import starling.browser._
import starling.gui._
import api._
import pages.{ValuationParametersPage, UserDetailsPage}
import swing.Publisher
import starling.bouncyrmi.{BouncyRMI, BouncyRMIClient}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.pivot.{Field, SomeSelection}
import starling.tradestore.TradePredicate
import starling.daterange.{Day, TimeOfDay}
import collection.immutable.TreeSet
import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.trade.facility.TradeFacility
import starling.reports.facility.ReportFacility
import starling.manager.{ServiceProperties, HttpContext, BromptonContext, BromptonActivator}

case class GuiLaunchParameters(serverRmiHost:String, serverRmiPort:Int, principalName:String, runAs:Option[String])

class GuiBromptonActivator extends BromptonActivator {
  var client:BouncyRMIClient = _

  def start(context: BromptonContext) {
    val guiLaunchParameters = context.awaitService(classOf[GuiLaunchParameters])
    System.setProperty(BouncyRMI.CodeVersionKey, BouncyRMI.CodeVersionUndefined)
    val overriddenUser = guiLaunchParameters.runAs
    client = new BouncyRMIClient(guiLaunchParameters.serverRmiHost, guiLaunchParameters.serverRmiPort, GuiStart.auth(guiLaunchParameters.principalName), overriddenUser = overriddenUser)
    client.startBlocking
    val starlingServer = client.proxy(classOf[StarlingServer])
    val tradeService = client.proxy(classOf[TradeFacility])
    starlingServer.storeSystemInfo(GuiStart.systemInfo)

    val fc2Service = client.proxy(classOf[FC2Facility])
    val reportService = client.proxy(classOf[ReportFacility])
    val rabbitEventViewerService = client.proxy(classOf[RabbitEventViewerService])

    context.registerService(classOf[Publisher], client.remotePublisher)
    context.registerService(classOf[StarlingServer], starlingServer)
    context.registerService(classOf[FC2Facility], fc2Service)
    context.registerService(classOf[RabbitEventViewerService], rabbitEventViewerService)
    context.registerService(classOf[ReportFacility], reportService)
    context.registerService(classOf[TradeFacility], tradeService)
    context.registerService(classOf[BrowserService], client.proxy(classOf[BrowserService]))
    context.registerService(classOf[BrowserBundle], new StarlingBrowserBundle(starlingServer, reportService, fc2Service, rabbitEventViewerService, tradeService))


    context.registerService(classOf[HttpServlet], new HttpServlet {
      override def doGet(req:HttpServletRequest, resp:HttpServletResponse) {
        val tradeID = req.getParameter("tradeID")
        val snapshotID = req.getParameter("snapshotID")

        val desk = Desk.Titan
        val tradeTimestamp = tradeService.deskCloses.get(desk).map(closes => closes.values.flatten.toList.sortWith(_.timestamp > _.timestamp)).get.head
        val tradePredicate = TradePredicate(List(), List(List((Field("Trade ID"), SomeSelection(Set(tradeID))))))
        val tradeSelection = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), tradePredicate, None)

        val curveIdentifier = {
          val pricingGroup = PricingGroup.Metals
          val marketDataSelection = MarketDataSelection(Some(pricingGroup))
          val version = snapshotID.toInt
          val enRule = EnvironmentRuleLabel.AllCloses
          import EnvironmentModifierLabel._
          val envMods = TreeSet[EnvironmentModifierLabel]() + zeroInterestRates
          val ci = CurveIdentifierLabel.defaultLabelFromSingleDay(MarketDataIdentifier(marketDataSelection, version), starlingServer.ukBusinessCalendar)
          ci.copy(thetaDayAndTime = ci.thetaDayAndTime.copyTimeOfDay(TimeOfDay.EndOfDay), environmentRule = enRule, envModifiers = envMods)
        }

        val slidableReportOptions = reportService.reportOptionsAvailable.options.filter(_.slidable)
        val reportOptions = new ReportOptions(slidableReportOptions, None, None)

        val tradeIDLabel = TradeIDLabel(tradeID, TradeSystemLabel("Titan", "ti"))
        val rp = ReportParameters(tradeSelection, curveIdentifier, reportOptions, Day.today, None, true)

        client.remotePublisher.publish(GotoPageEvent(ValuationParametersPage(tradeIDLabel, rp)))
      }
    }, ServiceProperties(HttpContext("gotoValuationScreen")))
  }

  override def stop(context: BromptonContext) {
    if (client != null) client.stop()
  }
}

import StarlingLocalCache._
class StarlingBrowserBundle(
                             starlingServer:StarlingServer,
                             reportService:ReportFacility,
                             fc2Service:FC2Facility,
                             rabbitEventService:RabbitEventViewerService,
                             tradeService:TradeFacility) extends BrowserBundle {
  def bundleName = "StarlingServer"
  def marshal(obj: AnyRef) = GuiStarlingXStream.write(obj)
  override def userPage(context:PageContext) = Some( UserDetailsPage(context.localCache.currentUser) )

  def initCache(cache: HeterogeneousMap[LocalCacheKey], publisher:Publisher) {
    GuiStart.initCacheMap(cache, starlingServer, reportService, fc2Service, tradeService, rabbitEventService, publisher)
  }

  override def notificationHandlers = StarlingServerNotificationHandlers.notificationHandler :: Nil
  def unmarshal(text: String) = GuiStarlingXStream.read(text).asInstanceOf[AnyRef]

//            bookmark match {
//              case rb:ReportBookmark => {
//                val userReportData = rb.userReportData
//                val pivotLayout = rb.pivotPageState.pivotFieldParams.pivotFieldState match {
//                  case None => throw new Exception("I should have a layout at this stage")
//                  case Some(pfs) => PivotLayout(name, pfs, true, rb.pivotPageState.otherLayoutInfo, "special", Nil)
//                }
//                starlingServer.saveUserReport(name, userReportData, showParameters)
//                if (shouldSaveLayout && shouldAssociateLayout) {
//                  // This is the case where it is a custom layout so we want to save the layout and associate it with this report
//                  starlingServer.saveLayout(pivotLayout.copy(associatedReports = List(name)))
//                } else if (shouldAssociateLayout) {
//                  // This is the case where the layout is already saved but we want to associate it with this report.
//                  starlingServer.deleteLayout(pivotLayout.layoutName)
//                  starlingServer.saveLayout(pivotLayout.copy(associatedReports = name :: pivotLayout.associatedReports))
//                }
//              }
//            }

  override def settings(pageContext:PageContext) = StarlingSettings.create(pageContext)
  override def homeButtons(pageContext:PageContext) = StarlingHomeButtons.create(pageContext)
  override def utilButtons(pageContext:PageContext) = StarlingUtilButtons.create(pageContext)
  override def helpEntries = StarlingHelpPage.starlingHelpEntry :: Nil
}