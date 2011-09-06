package starling.gui.osgi

import starling.rmi.StarlingServer
import starling.fc2.api.FC2Service
import starling.browser.service.BrowserService
import starling.gui.xstream.GuiStarlingXStream
import javax.swing.KeyStroke
import java.awt.event.{InputEvent, KeyEvent}
import starling.browser.service.internal.HeterogeneousMap
import starling.browser._
import starling.gui._
import api._
import pages.{ValuationParametersPage, UtilsPage, UserDetailsPage}
import swing.Publisher
import starling.bouncyrmi.{BouncyRMI, BouncyRMIClient}
import starling.manager.{HttpContext, BromptonContext, BromptonActivator}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.pivot.{Field, SomeSelection}
import starling.tradestore.TradePredicate
import starling.daterange.{Day, TimeOfDay}
import collection.immutable.TreeSet

class GuiBromptonProps {
  def serverHostname:String = "localhost"

  def rmiPort:Int = 8881
  def principalName:String = "STARLING-TEST/dave-linux"
}
class GuiBromptonActivator extends BromptonActivator {
  type Props = GuiBromptonProps

  def defaults = new GuiBromptonProps

  var client:BouncyRMIClient = _

  def start(context: BromptonContext) { }

  def init(context: BromptonContext, props: GuiBromptonProps) {

    System.setProperty(BouncyRMI.CodeVersionKey, BouncyRMI.CodeVersionUndefined)
    client = new BouncyRMIClient(props.serverHostname, props.rmiPort, GuiStart.auth(props.principalName), classLoader=getClass.getClassLoader)
    client.startBlocking()
    val starlingServer = client.proxy(classOf[StarlingServer])
    starlingServer.storeSystemInfo(GuiStart.systemInfo)

    val fc2Service = client.proxy(classOf[FC2Service])
    context.registerService(classOf[Publisher], client.remotePublisher)
    context.registerService(classOf[StarlingServer], starlingServer)
    context.registerService(classOf[FC2Service], fc2Service)
    context.registerService(classOf[BrowserService], client.proxy(classOf[BrowserService]))
    context.registerService(classOf[BrowserBundle], new StarlingBrowserBundle(starlingServer, fc2Service))

    context.registerService(classOf[HttpServlet], new HttpServlet {
      override def doGet(req:HttpServletRequest, resp:HttpServletResponse) {
        val tradeID = req.getParameter("tradeID")
        val snapshotID = req.getParameter("snapshotID")

        val desk = Desk.Titan
        val tradeTimestamp = starlingServer.deskCloses.get(desk).map(closes => closes.values.flatten.toList.sortWith(_.timestamp > _.timestamp)).get.head
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

        val slidableReportOptions = starlingServer.reportOptionsAvailable.options.filter(_.slidable)
        val reportOptions = new ReportOptions(slidableReportOptions, None, None)

        val tradeIDLabel = TradeIDLabel(tradeID, TradeSystemLabel("Titan", "ti"))
        val rp = ReportParameters(tradeSelection, curveIdentifier, reportOptions, Day.today, None, true)

        client.remotePublisher.publish(GotoPageEvent(ValuationParametersPage(tradeIDLabel, rp)))
      }
    }, List(HttpContext("gotoValuationScreen")))
  }

  def stop(context: BromptonContext) {
    if (client != null) client.stop()
  }
}

import StarlingLocalCache._
class StarlingBrowserBundle(starlingServer:StarlingServer, fc2Service:FC2Service) extends BrowserBundle {
  def bundleName = "StarlingServer"
  def marshal(obj: AnyRef) = GuiStarlingXStream.write(obj)
  override def userPage(context:PageContext) = Some( UserDetailsPage(context.localCache.currentUser) )
  override def hotKeys = HotKey(
    KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK),
    "utilsPage",
    UtilsPage()) :: Nil


  def initCache(cache: HeterogeneousMap[LocalCacheKey], publisher:Publisher) {
    GuiStart.initCacheMap(cache, starlingServer, fc2Service, publisher)
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
  override def helpEntries = StarlingHelpPage.starlingHelpEntry :: Nil
}