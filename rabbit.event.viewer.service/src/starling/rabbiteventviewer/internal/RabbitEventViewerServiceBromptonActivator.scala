package starling.rabbiteventviewer.internal

import starling.rabbiteventviewer.api.RabbitEventViewerService
import starling.manager.{ExportGuiRMIProperty, BromptonContext, BromptonActivator}
import starling.rmi.RabbitEventDatabase

class RabbitEventViewerProps

class RabbitEventViewerServiceBromptonActivator extends BromptonActivator {
  type Props = RabbitEventViewerProps
  def defaults = new RabbitEventViewerProps
  def init(context:BromptonContext, props:RabbitEventViewerProps) {
    val eventDatabase = context.awaitService(classOf[RabbitEventDatabase])
    context.registerService(classOf[RabbitEventViewerService], new RabbitEventViewerServiceImpl(eventDatabase), ExportGuiRMIProperty::Nil)
  }
  def start(context:BromptonContext) {}
  def stop(context:BromptonContext) {}
}

