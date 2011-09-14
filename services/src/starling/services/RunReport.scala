package starling.services

import starling.daterange.Day
import starling.concurrent.MP
import starling.props.PropsHelper
import starling.auth.{AuthHandler, User}
import starling.utils.{Broadcaster, Log}

object RunReport {
  def main(args:Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, AuthHandler.Dev, Broadcaster.Null, false, false, false, false)
    init.start
    init.userReportsService.runNamedReport(User.Dev, "AAA", Day(2011, 2, 14), None)
    println("")
    println("<----------->")
    Thread.sleep(10000)
    Log.infoWithTime("Running again") {
      init.userReportsService.runNamedReport(User.Dev, "AAA", Day(2011, 2, 15), None)
    }
    println("Done")
    init.stop
  }
}
