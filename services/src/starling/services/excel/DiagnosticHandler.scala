package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.daterange._
import starling.auth.User
import starling.rmi.StarlingServer
import starling.loopyxl.ExcelMethod

class DiagnosticHandler(starlingServer: StarlingServer) {
  @ExcelMethod
  @XLFunction(name = "testConnection", category = "Starling")
  def testConnection(): String = "OK: " + new Timestamp().timeStringWithSeconds

  @ExcelMethod
  @XLFunction(name = "serverName", category = "Starling")
  def serverName(): String = starlingServer.name

  @ExcelMethod
  @XLFunction(name = "user", category = "Starling")
  def user(): String = User.currentlyLoggedOn match {
    case null => "# Error getting user: null"
    case user => user.name
  }
}