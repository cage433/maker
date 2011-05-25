package starling.utils

import org.testng.ITestResult
import org.testng.TestListenerAdapter

/**
   A test listener for SBT
*/
class SBTTestListener extends TestListenerAdapter {

	override def onTestFailure(tr : ITestResult) {
		super.onTestFailure(tr)
		println("\nTest failed " + tr)
		tr.getThrowable().printStackTrace()
	}
	
	override def onTestStart(tr : ITestResult){
		super.onTestStart(tr)
    // Uncomment these if a test hangs
//		println("Running " + tr.getMethod())
//		print(".")
	}
	
}
