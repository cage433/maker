package starling.gui

import java.util.concurrent.{Callable, FutureTask, ConcurrentHashMap}
import starling.rmi.{StarlingServer}
import java.lang.reflect.{Method, InvocationHandler, Proxy}
import starling.utils.cache.ThreadSafeCachingProxy

trait SubmitRequest[R] {
  def submit(server:StarlingServer):R
}
class PageBuildingContext(val starlingServer:StarlingServer) {
  val cachingStarlingServer:StarlingServer = ThreadSafeCachingProxy.createProxy(starlingServer, classOf[StarlingServer])
  def submit[R](submitRequest: SubmitRequest[R]):R = {
    submitRequest.submit(starlingServer)
  }
}



