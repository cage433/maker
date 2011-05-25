package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.ReflectFunctionHandler
import org.boris.xlloop.handler.{FunctionInformationHandler, CompositeFunctionHandler}
import org.boris.xlloop.{IFunctionContext, IFunctionHandler, FunctionServer}
import starling.auth.{LdapUserLookup, User}
import org.boris.xlloop.xloper.{XLError, XLString, XLoper}
import java.net.SocketException
import starling.utils.{Stoppable, Log}

/**
 * Locally store curves uploaded from xlloop and hand them out as price data.
 */
class ExcelLoopReceiver(ldapUser: LdapUserLookup, port: Int, handlers: Object*) extends Stoppable with Runnable {
  val functionServer = new FunctionServer(port)

  def start {
    new Thread(this).start
  }

  def stop() = {
     functionServer.stop()
  }

  def run = {

    val reflect = new ReflectFunctionHandler
    handlers.map {
      handler => reflect.addMethods("Starling.", handler)
    }

    val functionInfo = new FunctionInformationHandler
    functionInfo.add(reflect)

    val composite = new CompositeFunctionHandler
    composite.add(reflect)
    composite.add(functionInfo)
    val exceptionWrapper = new IFunctionHandler() {
      def hasFunction(f: String) = composite.hasFunction(f)

      def execute(context: IFunctionContext, name: String, args: Array[XLoper]): XLoper = {
        for (arg <- args) {
          if (arg.`type` == XLoper.xlTypeErr || (arg.`type` == XLoper.xlTypeStr && arg.asInstanceOf[XLString].str.startsWith("#"))) {
            Log.error("Error parsing excel args: " + (name, args.toSeq, arg))
            if (arg.`type` == XLError.NA.`type`)
              return new XLString("#Unknown Error (possible string > 255 characters?)")
            else
              return new XLString("#Error" + arg)
          }
        }
        try {
          name match {
            case "org.boris.xlloop.Initialize" => {
              val userName = args(0).asInstanceOf[XLString].str
              ldapUser.user(userName) match {
                case user@Some(_) => {
                  User.setLoggedOn(user)
                  args(0)
                }
                case None => {
                  Log.warn("# Unrecognised username: " + userName)
                  new XLString("# Unrecognised username: " + userName)
                }
              }
            }
            case _ => {
              User.currentlyLoggedOn match {
                case null => throw new Exception("Unable to run excel command as user cannot be determined")
                case user => composite.execute(context, name, args)
              }

            }
          }

        } catch {
          case e: Exception => {
            Log.warn("Exception invoking excel function: " + name + " " + args.toList, e)
            new XLString("#" + e.getMessage)
          }
        }
      }
    }
    functionServer.setFunctionHandler(exceptionWrapper)

    println("XLLoop Thread listening on port " + functionServer.getPort + "...")
    try {
      functionServer.run
    } catch {
      case e:SocketException if (e.getMessage == "Socket closed") => {
        //Do nothing. This happens when xlloop is stopped
      }
    }
  }
}