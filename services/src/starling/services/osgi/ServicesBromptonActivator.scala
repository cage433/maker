package starling.services.osgi

import starling.props.PropsHelper
import starling.services.{StarlingInit}
import starling.auth.AuthHandler
import com.trafigura.services.valuation.ValuationServiceApi
import com.trafigura.services.marketdata.MarketDataServiceApi
import starling.fc2.api.FC2Facility
import starling.browser.service.BrowserService
import starling.manager._
import starling.utils.{Broadcaster}
import starling.db.{MarketDataStore, DB}
import starling.tradestore.TradeStores
import starling.calendar.BusinessCalendars
import starling.rmi.{RabbitEventDatabase, UserSettingsDatabase, BromptonTrackerBasedMarketDataPageIdentifierReaderProviders, StarlingServer}
import starling.services.excel.ExcelLoopReceiver
import starling.loopyxl.ReflectiveMethodSource
import starling.curves.{VanillaEnvironmentRule, EnvironmentRule, EnvironmentRules, CurveViewer}
import starling.daterange.ObservationPoint._
import starling.daterange.{ObservationPoint, TimeOfDay, ObservationTimeOfDay}
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}
import starling.marketdata.ReferenceDataLookup
import starling.utils.ImplicitConversions._
import javax.servlet.http.HttpServlet

class ServicesBromptonActivator extends BromptonActivator {

  var starlingInit:StarlingInit = _
  var excelLoopReceiver:ExcelLoopReceiver = _

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
      marketDataReadersProviders = bromptonMarketDataReaderProviders
    )
    context.registerService(classOf[StarlingServer], starlingInit.starlingServer,ServiceProperties(ExportGuiRMIProperty))
    context.registerService(classOf[FC2Facility], starlingInit.fc2Service,ServiceProperties(ExportGuiRMIProperty))
    context.registerService(classOf[BrowserService], starlingInit.browserService,ServiceProperties(ExportGuiRMIProperty))


    context.registerService(classOf[UserSettingsDatabase], starlingInit.userSettingsDatabase)
    context.registerService(classOf[MarketDataStore], starlingInit.marketDataStore)
    context.registerService(classOf[BusinessCalendars], starlingInit.businessCalendars)
    context.registerService(classOf[CurveViewer], starlingInit.curveViewer)

    starlingInit.xlloopAndloopyReceivers.foreach { receiver => {
      context.registerService(classOf[AnyRef], receiver, ServiceProperties(ExportLoopyProperty, ExportXlloopProperty))
    }}
    context.registerService(classOf[AnyRef], starlingInit.curveHandler, ServiceProperties(ExportXlloopProperty))

    { // Environment Rules
      context.registerService(classOf[EnvironmentRules], starlingInit.environmentRules)
      context.createServiceTracker(Some(classOf[EnvironmentRule]), serviceTracker = new BromptonServiceCallback[EnvironmentRule] {
        def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, environmentRule: EnvironmentRule) = {
          starlingInit.environmentRules.add(environmentRule)
        }

        override def serviceRemoved(ref: BromptonServiceReference) = {
          throw new Exception("Not implemented")
        }
      })

      val everythingButMetals = PricingGroup.values - PricingGroup.Metals
      val Default = new VanillaEnvironmentRule(_.atTimeOfDay(ObservationTimeOfDay.Default), TimeOfDay.EndOfDay,
        EnvironmentRuleLabel.COB, everythingButMetals, starlingInit.referenceDataLookup)

      val RealTime = new VanillaEnvironmentRule(_ => ObservationPoint.RealTime, TimeOfDay.StartOfDay,
        EnvironmentRuleLabel.RealTime, everythingButMetals, starlingInit.referenceDataLookup)

      context.registerService(classOf[EnvironmentRule], Default)
      context.registerService(classOf[EnvironmentRule], RealTime)
    }

    context.registerService(classOf[ReferenceDataLookup], starlingInit.referenceDataLookup)

    excelLoopReceiver = new ExcelLoopReceiver(starlingInit.ldapUserLookup, props.XLLoopPort())
    context.createServiceTracker(None,  ServiceProperties(ExportXlloopProperty), new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, service: AnyRef) = {
        excelLoopReceiver.register(ref, service)
      }
      override def serviceRemoved(ref: BromptonServiceReference) = {
        excelLoopReceiver.unregister(ref)
      }
    })
    excelLoopReceiver.start

    starlingInit.start
    starlingInit.servlets.foreach { case (name, servlet) => context.registerService(classOf[HttpServlet], servlet, ServiceProperties(HttpContext(name))) }
  }

  override def stop(context: BromptonContext) {
    if (starlingInit != null) {
      starlingInit.stop
    }
    if (excelLoopReceiver != null) {
      excelLoopReceiver.stop
    }
  }
}