package starling.services

import starling.daterange.Day
import starling.concurrent.MP
import starling.utils.Log
import starling.auth.User
import starling.props.PropsHelper

object RunReport {
  def main(args:Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, false, false, false, false)
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
