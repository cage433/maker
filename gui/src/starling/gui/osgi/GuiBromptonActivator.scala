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
import starling.bouncyrmi.{BouncyRMI, BouncyRMIClient}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import starling.pivot.{Field, SomeSelection}
import starling.tradestore.TradePredicate
import starling.daterange.{Day, TimeOfDay}
import collection.immutable.TreeSet
import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.trade.facility.TradeFacility
import starling.reports.facility.ReportFacility
import starling.services.EmailService
import starling.calendar.BusinessCalendar
import swing.{Swing, Publisher}
import java.util.Date
import starling.manager._

class GuiBromptonActivator extends BromptonActivator {
  def start(context: BromptonContext) {
    val starlingServer = context.awaitService(classOf[StarlingServer])
    val tradeService = context.awaitService(classOf[TradeFacility])
    val thread = new Thread(new Runnable() { def run() {
      starlingServer.storeSystemInfo(GuiStart.systemInfo)
    }})
    thread.setName("storeSystemInfo")
    thread.start

    val fc2Service = context.awaitService(classOf[FC2Facility])
    val reportService = context.awaitService(classOf[ReportFacility])
    context.registerService(classOf[BrowserBundle], new StarlingBrowserBundle(starlingServer, reportService, fc2Service, tradeService))

    val broadcaster = context.awaitService(classOf[Broadcaster])
    val localCache = context.awaitService(classOf[LocalCache])

    context.registerService(classOf[HttpServlet], new HttpServlet {
      override def doGet(req:HttpServletRequest, resp:HttpServletResponse) {
        req.getParameter("tradeID") match {
          case tradeID:String => {
            showTrade(tradeID)
            resp.setContentType("text/plain")
            resp.setStatus(200)
            resp.getWriter.println("Openned valuation page for " + tradeID + " at " + new Date())
          }
          case _ => {
            resp.setContentType("text/plain")
            resp.setStatus(404)
            resp.getWriter.println("You need to specify the tradeID parameter - it is case sensitive")
          }
        }
      }
    }, ServiceProperties(HttpContext("gotoValuationScreen")))

    def showTrade(tradeID:String) {
      val desk = Desk.Titan
      import StarlingLocalCache._
      var deskCloses: List[TradeTimestamp] = null
      var marketDataVersion:Int = -1
      val pricingGroup = PricingGroup.Metals
      val marketDataSelection = MarketDataSelection(Some(pricingGroup))
      Swing.onEDTWait( {
        deskCloses = localCache.deskCloses(desk);
        marketDataVersion = localCache.latestMarketDataVersion(marketDataSelection)
      })
      val tradeTimestamp = deskCloses.sortWith(_.timestamp > _.timestamp).head
      val tradePredicate = TradePredicate(List(), List(List((Field("Trade ID"), SomeSelection(Set(tradeID))))))
      val tradeSelection = TradeSelectionWithTimestamp(Some((desk, tradeTimestamp)), tradePredicate, None)

      val curveIdentifier = {
        val observationDay = Day.today.previousWeekday.endOfDay
        val mods = TreeSet[EnvironmentModifierLabel](EnvironmentModifierLabel.zeroInterestRates)

        CurveIdentifierLabel(
          MarketDataIdentifier(marketDataSelection, marketDataVersion),
          EnvironmentRuleLabel.AllCloses,
          observationDay,
          Day.today.endOfDay,
          Day.today.nextWeekday.atTimeOfDay(TimeOfDay.EndOfDay),
          mods
        )
      }

      val slidableReportOptions = reportService.reportOptionsAvailable.options.filter(_.slidable)
      val reportOptions = new ReportOptions(slidableReportOptions, None, None)

      val tradeIDLabel = TradeIDLabel(tradeID, TradeSystemLabel("Titan", "ti"))
      val rp = ReportParameters(tradeSelection, curveIdentifier, reportOptions, Day.today, None, true)

      broadcaster.broadcast(GotoPageEvent(ValuationParametersPage(tradeIDLabel, rp, ReportSpecificChoices(), true)))
    }
  }
}

object StarlingBrowserBundle {
  val BundleName = "StarlingServer"
}

import StarlingLocalCache._
class StarlingBrowserBundle(
                             starlingServer:StarlingServer,
                             reportService:ReportFacility,
                             fc2Service:FC2Facility,
                             tradeService:TradeFacility) extends BrowserBundle {
  def bundleName = StarlingBrowserBundle.BundleName
  def marshal(obj: AnyRef) = GuiStarlingXStream.write(obj)
  override def userPage(context:PageContext) = Some( UserDetailsPage(context.localCache.currentUser) )

  override def initCache() = GuiStart.initCache(starlingServer, fc2Service, reportService, tradeService)
  override def addListeners(cache: HeterogeneousMap[LocalCacheKey], publisher:Publisher) {
    GuiStart.addListeners(cache, starlingServer, reportService, fc2Service, tradeService, publisher)
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