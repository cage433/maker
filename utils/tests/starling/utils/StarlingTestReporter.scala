package starling.utils

import org.scalatest.events._

class StarlingTestReporter() extends org.scalatest.Reporter{
  var timeAtStart : Long = 0
  def apply(event :Event){
    event match {
      case ts : TestStarting => timeAtStart = ts.timeStamp; 
      case ts : TestSucceeded => println( ts.testName + ", " + (ts.timeStamp - timeAtStart) + " (ms)")
      case _ =>
    }
  }

}
