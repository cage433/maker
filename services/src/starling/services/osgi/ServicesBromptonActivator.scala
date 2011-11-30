package starling.services.osgi

import starling.auth.AuthHandler
import starling.fc2.api.FC2Facility
import starling.browser.service.BrowserService
import starling.manager._
import starling.calendar.BusinessCalendars
import starling.services.excel.ExcelLoopReceiver
import starling.curves.{VanillaEnvironmentRule, EnvironmentRule, EnvironmentRules, CurveViewer}
import starling.daterange.{ObservationPoint, TimeOfDay, ObservationTimeOfDay}
import starling.gui.api.{PricingGroup, EnvironmentRuleLabel}
import starling.marketdata.ReferenceDataLookup
import javax.servlet.http.HttpServlet
import starling.rmi._
import starling.db.{MarketDataSource, MarketDataStore}
import starling.utils.{Receiver, Broadcaster}
import starling.services.{QlikViewUpdater, EmailService, ReferenceData, StarlingInit}
import starling.http.GUICode

class ServicesBromptonActivator extends BromptonActivator {
  def start(context: BromptonContext) {
    val authHandler = context.awaitService(classOf[AuthHandler])
    val osgiBroadcaster = context.awaitService(classOf[Broadcaster])

    val props = context.awaitService(classOf[starling.props.Props])

    val bromptonMarketDataReaderProviders = new BromptonTrackerBasedMarketDataPageIdentifierReaderProviders(context)

    val starlingInit = new StarlingInit(
      props,
      authHandler, osgiBroadcaster,
      true, true, true,
      forceGUICompatability=false,
      startEAIAutoImportThread=props.ImportsBookClosesFromEAI(),
      marketDataReadersProviders = bromptonMarketDataReaderProviders
    )
    context.registerService(classOf[String], GUICode.allMD5s)

    context.registerService(classOf[StarlingServer], starlingInit.starlingServer,ServiceProperties(ExportGuiRMIProperty))
    context.registerService(classOf[FC2Facility], starlingInit.fCFacility,ServiceProperties(ExportGuiRMIProperty))
    context.registerService(classOf[FC2Service], starlingInit.fC2Service)
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
      context.createServiceTracker(Some(classOf[EnvironmentRule]), tracker = new BromptonServiceCallback[EnvironmentRule] {
        def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, environmentRule: EnvironmentRule) = {
          starlingInit.environmentRules.add(environmentRule)
        }

        override def serviceRemoved(ref: BromptonServiceReference) = {
          throw new Exception("Not implemented")
        }
      })

      val everythingButMetals = PricingGroup.values filterNot(_ == PricingGroup.Metals)
      val Default = new VanillaEnvironmentRule(_.atTimeOfDay(ObservationTimeOfDay.Default), TimeOfDay.EndOfDay,
        EnvironmentRuleLabel.COB, everythingButMetals, starlingInit.referenceDataLookup, starlingInit.dataTypes)

      val RealTime = new VanillaEnvironmentRule(_ => ObservationPoint.RealTime, TimeOfDay.StartOfDay,
        EnvironmentRuleLabel.RealTime, everythingButMetals, starlingInit.referenceDataLookup, starlingInit.dataTypes)

      context.registerService(classOf[EnvironmentRule], Default)
      context.registerService(classOf[EnvironmentRule], RealTime)
    }

    context.registerService(classOf[ReferenceDataLookup], starlingInit.referenceDataLookup)
    context.registerService(classOf[EmailService], starlingInit.emailService, ServiceProperties(ExportGuiRMIProperty))

    context.createServiceTracker(Some(classOf[ReferenceData]), ServiceProperties(), new BromptonServiceCallback[ReferenceData] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, referenceData: ReferenceData) = {
        starlingInit.referenceDataService.add(referenceData)
      }
    })

    context.createServiceTracker(Some(classOf[MarketDataSource]), tracker = new BromptonServiceCallback[MarketDataSource] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, marketDataSource: MarketDataSource) = {
        starlingInit.marketDataSources += marketDataSource
      }
    })

    val excelLoopReceiver = new ExcelLoopReceiver(starlingInit.ldapUserLookup, props.XLLoopPort())
    context.createServiceTracker(None,  ServiceProperties(ExportXlloopProperty), new BromptonServiceCallback[AnyRef] {
      def serviceAdded(ref: BromptonServiceReference, properties:ServiceProperties, service: AnyRef) = {
        excelLoopReceiver.register(ref, service)
      }
      override def serviceRemoved(ref: BromptonServiceReference) = {
        excelLoopReceiver.unregister(ref)
      }
    })
    excelLoopReceiver.start

    if (props.QlikViewEnabled()) {
      context.registerService(classOf[Receiver], new QlikViewUpdater(props.QlikViewServerUrl(), props.QlikViewSpotFXTask()))
    }

    starlingInit.start
    starlingInit.servlets.foreach { case (name, servlet) => context.registerService(classOf[HttpServlet], servlet, ServiceProperties(HttpContext(name))) }

    context.onStopped {
      starlingInit.stop
      excelLoopReceiver.stop
    }
  }
}