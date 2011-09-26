package starling.props.internal

import starling.manager.{BromptonContext, BromptonActivator}
import starling.props.PropsHelper

class DelMe
class PropsBromptonActivator extends BromptonActivator {

  type Props = DelMe
  def defaults = new DelMe
  def init(context: BromptonContext, props: DelMe) {}

  def start(context: BromptonContext) {
    val props = PropsHelper.defaultProps
    context.registerService(classOf[starling.props.Props], props)
  }

  def stop(context: BromptonContext) {}
}