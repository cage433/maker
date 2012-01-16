package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import swing.event.Event
import collection.mutable.ListBuffer

class TypedBroadcasterTests extends WordSpec with ShouldMatchers {
  "should only dispatch events that derive from type" in {
    val thingsBroadcast = new ListBuffer[Thing]

    val broadcaster = ThingBroadcaster(thingsBroadcast)
    broadcaster.broadcast(new NonThing)
    broadcaster.broadcast(new RealThing)
    broadcaster.broadcast(new RealSpecialThing)

    thingsBroadcast.size should be === 2
  }

  private trait Thing
  private class RealThing extends Event with Thing
  private class RealSpecialThing extends RealThing
  private class NonThing extends Event

  private case class ThingBroadcaster(thingsBroadcasted: ListBuffer[Thing]) extends TypedBroadcaster[Thing] {
    def typedBroadcast(thing: Thing) = thingsBroadcasted += thing
  }
}