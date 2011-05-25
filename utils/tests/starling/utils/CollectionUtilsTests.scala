package starling.utils

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._

class CollectionUtilsTests extends TestNGSuite {
  import CollectionUtils._
  
	@Test
	def testSetUnionWithSets{
	  val set1 = Set[Int](1, 2, 3)
	  val set2 = Set[Int](3, 4, 5)
	  val union = setUnion(List(set1, set2))
	  val expectedUnion = Set[Int](1, 2, 3, 4, 5)
	  assertEquals(expectedUnion, union)
	}
  
  @Test
  def testSetUnionWithLists{
	  val list1 = List(1, 2, 3, 2, 3)
	  val list2 = List(3, 4, 5, 3, 3)
	  val union = setUnion(Set(list1, list2))
	  val expectedUnion = Set[Int](1, 2, 3, 4, 5)
	  assertEquals(expectedUnion, union)
  }

  @Test
  def testSplit {
    val list1 = (0 to 10).toList
    assertEquals(CollectionUtils.split(list1, 2), List(List(0, 1), List(2, 3), List(4, 5), List(6, 7), List(8, 9), List(10)))
    val list2 = (0 to 9).toList
    assertEquals(CollectionUtils.split(list2, 2), List(List(0, 1), List(2, 3), List(4, 5), List(6, 7), List(8, 9)))
  }

  @DataProvider(name ="testAllKeyValueCombinations")
  def testAllKeyValueCombinations : Array[Array[List[List[String]]]] = {
    Array(
      Array(List[List[String]]()),
      Array(List(
        List("Gold", "Copper", "Aluminium"),
        List("Portfolio1", "Portfolio2")
      )),
      Array(List(
        List(),
        List("Portfolio1", "Portfolio2")
      )),
      Array(List(
        List(),
        List()
      )),
      Array(List(
        List("Gold", "Copper", "Aluminium"),
        List("Portfolio1", "Portfolio2"),
        List("Bill", "Mike", "Jim")
      ))


    )
  }

  @Test(dataProvider="testAllKeyValueCombinations")
  def testAllKeyValueCombinations(keysAndValues : List[List[String]]){
    val explodedValues : List[List[String]] = allKeyValueCombinations[String](keysAndValues)

    // Test that the number of combinations is the product of the number of values for each key
    val expectedSize = if (keysAndValues.isEmpty) 0 else (1 /: keysAndValues.map(_.size))(_*_)
    assertEquals(expectedSize, explodedValues.size)


    // check that exploded values preserve order
    keysAndValues.zipWithIndex.foreach{
      case (list, i) =>
        explodedValues.map(_(i)).distinct == keysAndValues(i)
    }
  }

  @Test
  def testFindDefined {
    val integers : List[Int] = (0 to 10).toList
    assertEquals(findDefined[Int,Int]((i) => if (i == 3) Some(i + 10) else None, integers), Some(13))
    assertEquals(findDefined[Int,Int]((i) => Some(i), integers), Some(0))
    assertEquals(findDefined[Int,Int]((i) => if (i > 30) Some(i) else None, integers), None)
  }

  @Test
  def testFirstGtr {
    assertEquals(firstGtr(List(0,1,2,-2,3,-1,4,5,6), 3), 6)
  }

  @Test
  def testFindNearest {
    assertEquals(findNearest(List(10,-2,2,1,5,3,-1), 5), 4)
    assertEquals(findNearest(List(10,-2,2,1,5,3,-1), 6), 4)
    assertEquals(findNearest(List(10,-2,2,1,5,3,-1), 9), 0)
    assertEquals(findNearest(List(10,-2,2,1,5,3,-1), -1), 6)
    assertEquals(findNearest(List(10,-2,2,1,5,3,-1), -3), 1)
  }

  @Test
  def testLevenshtein {
    assertEquals(levenshtein("dave".toArray, "dave".toArray), 0)
    assertEquals(levenshtein("dave".toArray, "Dave".toArray), 1)
    assertEquals(levenshtein("ave".toArray, "Dave".toArray), 1)
    assertEquals(levenshtein("dave".toArray, "ave".toArray), 1)
    assertEquals(levenshtein("davE".toArray, "daVe".toArray), 2)
    assertEquals(levenshtein("DAVE".toArray, "dave".toArray), 4)
    assertEquals(levenshtein("da??ve".toArray, "dave".toArray), 2)
    assertEquals(levenshtein("".toArray, "Dave".toArray), 4)
    assertEquals(levenshtein("ve".toArray, "Dave".toArray), 2)
  }
}
