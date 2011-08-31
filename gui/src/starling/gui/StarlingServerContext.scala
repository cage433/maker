package starling.gui

import java.util.concurrent.{Callable, FutureTask, ConcurrentHashMap}
import starling.rmi.{StarlingServer}
import java.lang.reflect.{Method, InvocationHandler, Proxy}
import starling.utils.cache.ThreadSafeCachingProxy
import starling.browser.{ServerContext, SubmitRequest}

trait StarlingSubmitRequest[R] extends SubmitRequest[R] {
  def baseSubmit(serverContext:ServerContext) = {
    submit(new StarlingServerContext(serverContext.lookup(classOf[StarlingServer])))
  }
  def submit(server:StarlingServerContext):R
}

class StarlingServerContext(val server:StarlingServer) {
  val cachingStarlingServer:StarlingServer = ThreadSafeCachingProxy.createProxy(server, classOf[StarlingServer])
}



