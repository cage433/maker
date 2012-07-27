import Common._
import Utils._
import Starling._
import maker.Maker._


// runs the 'blotter' tests to prove/protect 'existing' functionality for Kudu replacement
// nb: these are not really tests ATM, just a main method that does some testing...
println("starling blotter tests...")

val br = launcher.runMain("starling.test.InstrumentUploadTest")(commonLaunchArgs : _*)()

println("Blotter tests have completed")

handleExit(br)

