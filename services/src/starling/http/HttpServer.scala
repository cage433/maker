package starling.http

import java.lang.String
import javax.servlet.Servlet
import java.util.EventListener
import starling.utils.{Stopable, Log}

class HttpServer(portNo : Int,
                 val externalURL : String,
                 serverName : String,
                 descriptor : Option[String],
                 listeners : List[EventListener],
                 servlets: (String, Servlet)*) extends Stopable with Log {
}