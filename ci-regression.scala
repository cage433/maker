import Starling._

// nb: these are not really tests ATM, just a main method that does some testing...
println("starling Bookmark tests...")

debug 

val r = launcher.runMain("starling.bookmarkedtests.BookmarkTests")(commonLaunchArgs : _*)()

println("Bookmark tests  completed")

handleExit(r)

