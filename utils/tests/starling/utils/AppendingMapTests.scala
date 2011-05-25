package starling.utils

import org.testng.Assert._
import org.testng.annotations.{DataProvider, Test}
import org.scalatest.testng.TestNGSuite

class AppendingMapTests extends TestNGSuite {

  @DataProvider(name = "testBehaviourProvider")
  def testBehaviourProvider : Array[Array[Map[Any, Any]]] = {
    Array(
      Array(Map(1 -> 2), Map(3 -> 4)),
      Array(Map(1 -> 2, 3 -> 4), Map()),
      Array(Map(), Map(1 -> 2, 3 -> 4)),
      Array(Map(), Map()),
      Array(Map(1 -> 2, 3 -> 4), Map(3 -> 5))
      )
  }
  
  @Test(dataProvider = "testBehaviourProvider")
  def testBehaviour(map1 : Map[Any, Any], map2 : Map[Any, Any]){
    val appended = new AppendingMap[Any, Any](Map("a"->map1, "b"->map2))
    val notAppended = map2 ++ map1
    notAppended.keys.foreach{
      k => assertEquals(notAppended(k), appended(k))
    }
  }

  @Test
  def testPerformance{
    var mutable = scala.collection.mutable.Map() ++ (1 to 100).toList.map{i => (i -> i * i)}
    var immutable = scala.collection.immutable.Map() ++ (1 to 100).toList.map{i => (i -> i * i)}


    val N = 1000000
    Log.infoWithTime("no lock"){
      for (i <- 0 to N){
        val foo = mutable + (100 -> 50)
      }
    }
    val lock = new Object()
    Log.infoWithTime("locked"){
      for (i <- 0 to N) {
        lock.synchronized{
          val foo = mutable + (100 -> 50)
        }
      }
    }
    Thread.sleep(1000)
  }
}