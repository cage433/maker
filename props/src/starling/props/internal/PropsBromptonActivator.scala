package starling.props.internal

import starling.manager.{BromptonContext, BromptonActivator}
import starling.props.PropsHelper

class PropsBromptonActivator extends BromptonActivator {

  def start(context: BromptonContext) {
    val props = PropsHelper.defaultProps
    context.registerService(classOf[starling.props.Props], props)
  }
}