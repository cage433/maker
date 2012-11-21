import Common._
import Utils._
import Starling._
import maker.Maker._


// runs the 'bookmark' tests
// nb: these are not really tests ATM, just a main method that does some testing...
println("starling Bookmark tests...")

val br = for {
  c <- starling.clean
  u <- starling.update
  b <- launcher.runMain("starling.bookmarkedtests.BookmarkTests")(lightweightLaunchArgs : _*)()
} yield b

println("Bookmark tests have completed")

handleExit(br)

