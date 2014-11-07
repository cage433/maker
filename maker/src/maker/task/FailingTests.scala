package maker.task

import java.util.concurrent.atomic.AtomicReference
import maker.utils.TestIdentifier
import maker.utils.TestFailure
import maker.utils.RichString._
import maker.utils.TableBuilder


object FailingTests{
  private var mostRecentFailures : List[(TestIdentifier, TestFailure)] = Nil

  def setFailures(failures : List[(TestIdentifier, TestFailure)]) = synchronized{
    mostRecentFailures = failures
  }

  def clear(){
    setFailures(Nil)
  }


  def report {
    if (mostRecentFailures.isEmpty){
      println("There were no failing tests".inBlue)
      return
    }
    val longform = mostRecentFailures.size < 5
    val tb = TableBuilder(
      "No  ",
      "Suite                   ",
      "Test                              ",
      "Message"
    )
    mostRecentFailures.zipWithIndex.foreach{
      case ((TestIdentifier(suite, _, test), TestFailure(message, _)), i) =>
        val msg = if (longform) message else message.take(30).replace("\n", " ") + "..."
        tb.addRow(i, suite, test, msg)
    }
    println("Failing Tests".inBlue)
    println(tb)
    println("\nEnter maker.task.FailingTests.failure(i) for more information on the i'th failing test\n\n".inRed)
  }

  def failure(i : Int){
    if (mostRecentFailures.isEmpty)
      println("There were no failing tests")
    else if (mostRecentFailures.size < i + 1)
      println("There were only " + mostRecentFailures.size + " failing tests")
    else {
      val (testID, testFailure) = mostRecentFailures(i)
      println(testFailure.formatted(testID.suiteClass))
    }
  }
}
