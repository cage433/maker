package starling.bouncyrmi

import starling.manager.{BromptonContext, BromptonActivator}

class BouncyRMIServerProps

class BouncyRMIBromptonXActivator extends BromptonActivator {
  type Props = BouncyRMIServerProps

  def defaults = new BouncyRMIServerProps

  def init(context: BromptonContext, props: BouncyRMIServerProps) {}

  def start(context: BromptonContext) {}

  def stop(context: BromptonContext) {}
}