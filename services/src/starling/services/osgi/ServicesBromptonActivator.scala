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
    context.registerService(classOf[StarlingServer], starlingInit.starlingServer,ExportGuiRMIProperty::Nil)
    context.registerService(classOf[FC2Facility], starlingInit.fc2Service,ExportGuiRMIProperty::Nil)
    context.registerService(classOf[BrowserService], starlingInit.browserService,ExportGuiRMIProperty::Nil)


    context.registerService(classOf[UserSettingsDatabase], starlingInit.userSettingsDatabase)
    context.registerService(classOf[MarketDataStore], starlingInit.marketDataStore)
    context.registerService(classOf[BusinessCalendars], starlingInit.businessCalendars)
    context.registerService(classOf[CurveViewer], starlingInit.curveViewer)

    starlingInit.xlloopAndloopyReceivers.foreach { receiver => {
      context.registerService(classOf[AnyRef], receiver, ExportLoopyProperty::ExportXlloopProperty::Nil)
    }}
    context.registerService(classOf[AnyRef], starlingInit.curveHandler, ExportXlloopProperty::Nil)

    { // Environment Rules
      context.registerService(classOf[EnvironmentRules], starlingInit.environmentRules)
      context.createServiceTracker(Some(classOf[EnvironmentRule]), serviceTracker = new BromptonServiceCallback[EnvironmentRule] {
        def serviceAdded(ref: BromptonServiceReference, environmentRule: EnvironmentRule) = {
          starlingInit.environmentRules.add(environmentRule)
        }

        def serviceRemoved(ref: BromptonServiceReference) = {
          throw new Exception("Not implemented")
        }
      })

      val everythingButMetals = PricingGroup.values - PricingGroup.Metals
      val Default = new VanillaEnvironmentRule((day)=>ObservationPoint(day, ObservationTimeOfDay.Default), TimeOfDay.EndOfDay,
        EnvironmentRuleLabel.COB, everythingButMetals)

      val RealTime = new VanillaEnvironmentRule((day)=>ObservationPoint.RealTime, TimeOfDay.StartOfDay,
        EnvironmentRuleLabel.RealTime, everythingButMetals)

      context.registerService(classOf[EnvironmentRule], Default)
      context.registerService(classOf[EnvironmentRule], RealTime)
    }

    excelLoopReceiver = new ExcelLoopReceiver(starlingInit.ldapUserLookup, props.XLLoopPort())
    context.createServiceTracker(None,  ExportXlloopProperty::Nil, new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, service: AnyRef) = {
        excelLoopReceiver.register(ref, service)
      }
      def serviceRemoved(ref: BromptonServiceReference) = {
        excelLoopReceiver.unregister(ref)
      }
    })
    excelLoopReceiver.start

    starlingInit.start
    //starlingInit.servlets.foreach { case (name, servlet) => context.registerService(classOf[HttpServlet], servlet, List(HttpContext(name)))}
  }

  def stop(context: BromptonContext) {
    if (starlingInit != null) {
      starlingInit.stop
    }
    if (excelLoopReceiver != null) {
      excelLoopReceiver.stop
    }
  }
}