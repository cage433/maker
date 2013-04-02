import Common._
import Utils._
import Starling._
import maker.Maker._


// runs the pnl bootstrap
println("Running PnL Bootstrap")

launcher.testCompile

val br = launcher.runMain("starling.pnlreconcile.PnlBootstrapLoader")(commonLaunchArgs : _*)()

println("PnL Bootstrap is completed")

handleExit(br)

