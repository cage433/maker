import starling.pivot._
val in = init
val ts = in.trinityTradeStore
val lts = in.trinityLazyTradeStore
var pr : PivotResult = null


def rl() = Log.infoWithTime("foo"){pr = lts.pivot(new Timestamp(), None, null, Nil).data(Nil, Nil, Nil, Nil); println("Size = " + pr.data.size); Nil}
