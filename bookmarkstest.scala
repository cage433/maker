import Common._
import Utils._
import Starling._
import maker.Maker._


// runs the 'bookmark' tests
// nb: these are not really tests ATM, just a main method that does some testing...
println("starling Bookmark tests...")

val br = launcher.runMain("starling.bookmarkedtests.BookmarkTests")(commonLaunchArgs : _*)()

println("Bookmark tests have completed")

handleExit(br)

