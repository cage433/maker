import Common._
import Utils._
import Starling._
import maker.Maker._


// runs the 'performance report' tests to check performance is stable from build to build
println("starling performance report tests...")

val br = for {
  c <- starling.clean
  u <- starling.update
  t <- launcher.testCompile
  b <- launcher.runMain("starling.test.regression.PerformanceReportTests")(lightweightLaunchArgs : _*)()
} yield b

println("Performance tests have completed")

handleExit(br)
