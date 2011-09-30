package starling.reports.impl

import pivot.{CurveIdentifierFactory, ReportServiceInternal, ReportContextBuilder}
import starling.tradestore.TradeStores
import starling.calendar.BusinessCalendars
import starling.reports.facility.ReportFacility
import starling.rmi.{MarketDataPageIdentifierReaderProvider, UserSettingsDatabase}
import starling.gui.api.{ReportMarketDataPageIdentifier, MarketDataPageIdentifier}
import starling.db.{DB, MarketDataStore}
import starling.manager.{ExportGuiRMIProperty, ExportXlloopProperty, BromptonContext, BromptonActivator}
import starling.curves.EnvironmentRules

class ReportsBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {
    val props = context.awaitService(classOf[starling.props.Props])
    val marketDataStore = context.awaitService(classOf[MarketDataStore])
    val tradeStores = context.awaitService(classOf[TradeStores])
    val businessCalendars = context.awaitService(classOf[BusinessCalendars])
    val userSettingsDatabase = context.awaitService(classOf[UserSettingsDatabase])
    val curveIdentifierFactory = new CurveIdentifierFactory(context.awaitService(classOf[EnvironmentRules]))
    val eaiStarlingDB = DB(props.StarlingDatabase())

    val reportContextBuilder = new ReportContextBuilder(marketDataStore)
    val reportServiceInternal = new ReportServiceInternal(reportContextBuilder, tradeStores, curveIdentifierFactory)
    val userReportsService = new UserReportsService(businessCalendars.UK, tradeStores, marketDataStore, userSettingsDatabase, reportServiceInternal)

    val reportService = new ReportFacilityImpl(reportServiceInternal, userReportsService, tradeStores, userSettingsDatabase, eaiStarlingDB, curveIdentifierFactory)

    val reportRecordingMarketDataReaderProvider = new MarketDataPageIdentifierReaderProvider() {
      def readerFor(identifier: MarketDataPageIdentifier) = {
        identifier match {
          case ReportMarketDataPageIdentifier(rp) => Some(reportServiceInternal.recordedMarketDataReader(rp))
          case _ => None
        }
      }
    }

    context.registerService(classOf[AnyRef], new ReportHandler(userReportsService), ExportXlloopProperty::Nil)
    context.registerService(classOf[ReportFacility], reportService, ExportGuiRMIProperty::Nil)
    context.registerService(classOf[MarketDataPageIdentifierReaderProvider], reportRecordingMarketDataReaderProvider)
  }

  def stop(context: BromptonContext) {}
}