package maker.task

import java.util.concurrent.atomic.AtomicReference
import maker.utils.TestIdentifier
import maker.utils.TestFailure
import maker.utils.RichString._
import maker.utils.TableBuilder


object FailingTests{
  private val last : AtomicReference[Option[List[(TestIdentifier, TestFailure)]]] = new AtomicReference(None)

  private def failures : List[(TestIdentifier, TestFailure)] = last.get.getOrElse(Nil)

  def report {
    if (failures.isEmpty){
      println("There were no failing tests".inBlue)
      return
    }
    val tb = TableBuilder(
      "No  ",
      "Suite                   ",
      "Test                              ",
      "Message"
    )
    failures.zipWithIndex.foreach{
      case ((TestIdentifier(suite, _, test), TestFailure(message, _)), i) => 
        tb.addRow(i, suite, test, message)
    }
    println("Failing Tests".inBlue)
    println(tb)
    println("\nEnter maker.task.FailingTests.report(i) for more information on the i'th failing test\n\n".inRed)
  }

  def report(i : Int){
    val (testID, testFailure) = failures(i)
    println(testFailure.formatted(testID.suiteClass))
  }
}
