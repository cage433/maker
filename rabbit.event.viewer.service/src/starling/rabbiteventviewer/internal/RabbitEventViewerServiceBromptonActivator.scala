package starling.rabbiteventviewer.internal

import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.rmi.RabbitEventDatabase
import starling.manager.{ServiceProperties, ExportGuiRMIProperty, BromptonContext, BromptonActivator}

class RabbitEventViewerServiceBromptonActivator extends BromptonActivator {
  def start(context:BromptonContext) {
    val eventDatabase = context.awaitService(classOf[RabbitEventDatabase])
    context.registerService(classOf[RabbitEventViewerService], new RabbitEventViewerServiceImpl(eventDatabase), ServiceProperties(ExportGuiRMIProperty))
  }
}

