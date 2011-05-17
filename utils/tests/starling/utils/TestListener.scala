package starling.utils

import org.testng.ITestResult
import org.testng.TestListenerAdapter

/**
   A test listener for Alex's emacs ide
*/
class TestListener extends TestListenerAdapter {


  override def onTestFailure(tr : ITestResult) {
    super.onTestFailure(tr)
    println("\nTest failed " + tr)
    tr.getThrowable().printStackTrace()
  }

  override def onTestStart(tr : ITestResult){
    super.onTestStart(tr)
    println("Running " + tr.getMethod())
    print(".")
  }

}
