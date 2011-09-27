package starling.rabbiteventviewer.internal

import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.manager.{ExportGuiRMIProperty, BromptonContext, BromptonActivator}
import starling.rmi.RabbitEventDatabase

class RabbitEventViewerServiceBromptonActivator extends BromptonActivator {
  def start(context:BromptonContext) {
    val eventDatabase = context.awaitService(classOf[RabbitEventDatabase])
    context.registerService(classOf[RabbitEventViewerService], new RabbitEventViewerServiceImpl(eventDatabase), ExportGuiRMIProperty::Nil)
  }
  def stop(context:BromptonContext) {}
}

