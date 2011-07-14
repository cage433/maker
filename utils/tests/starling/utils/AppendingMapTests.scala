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


}