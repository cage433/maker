import Common._
import Utils._
import Starling._
import maker.Maker._


println("starling Five Minute tests...")

val br = for {
  c <- starling.clean
  u <- starling.update
  t <- launcher.testCompile
  b <- launcher.runMain("starling.slowtests.FiveMinuteTests")(lightweightLaunchArgs : _*)()
} yield b

println("Five minute tests have completed")

handleExit(br)

