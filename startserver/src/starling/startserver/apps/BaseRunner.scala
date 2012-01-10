package starling.startserver.apps

import starling.auth.osgi.AuthBromptonActivator
import starling.services.osgi.ServicesBromptonActivator
import starling.reports.impl.ReportsBromptonActivator
import starling.singleclasspathmanager.SingleClasspathManager
import starling.manager.BromptonActivator
import starling.trade.impl.osgi.TradeBromptonActivator
import starling.utils.{SingleClasspathBroadcasterActivator, ThreadUtils}

trait Lookup {
  def apply[T](klass:Class[T]):T
}

object BaseRunner {

  private def runWith(activators:List[Class[_ <: BromptonActivator]])(f:(Lookup)=>Unit) {
    val single = new SingleClasspathManager(false, activators)
    single.start
    val lookup = new Lookup {
      def apply[T](klass: Class[T]) = single.service(klass)
    }
    f(lookup)
    single.stop
    ThreadUtils.printNonDaemonThreads
  }

  def runWithoutListening[T]()(lookup:Lookup=>Unit) {
    runWith(List(
      classOf[SingleClasspathBroadcasterActivator],
      classOf[AuthBromptonActivator],
      classOf[ServicesBromptonActivator],
      classOf[TradeBromptonActivator],
      classOf[ReportsBromptonActivator]
    ))(lookup)
  }
}