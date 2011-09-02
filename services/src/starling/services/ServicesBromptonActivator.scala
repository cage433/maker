package starling.services

import starling.props.PropsHelper
import starling.api.utils.PropertiesMapBuilder
import java.io.File
import javax.servlet.http.HttpServlet
import starling.manager.{HttpContext, BromptonContext, BromptonActivator}

class ServicesProps
class ServicesBromptonActivator extends BromptonActivator {

  type Props = ServicesProps
  def defaults = new ServicesProps

  var starlingInit:StarlingInit = _

  def start(context: BromptonContext) {
    val props:starling.props.Props = new starling.props.Props(PropertiesMapBuilder.propsFromFile(new File("props.conf")), Map())
    starlingInit = new StarlingInit(props, true, true, true, true, true, false, false, false)
    Server.server = starlingInit
    starlingInit.start
    //starlingInit.servlets.foreach { case (name, servlet) => context.registerService(classOf[HttpServlet], servlet, List(HttpContext(name)))}
  }

  def init(context: BromptonContext, props: ServicesProps) { }

  def stop(context: BromptonContext) {
    if (starlingInit != null) {
      starlingInit.stop
    }
  }
}