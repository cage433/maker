package starling.pivot.view.swing

import org.scalatest.testng.TestNGSuite
import org.testng.annotations._
import org.testng.Assert._
import starling.pivot.HasChildren
import starling.pivot.view.swing.MigTree._

case class TestTree(field:Int, children:List[TestTree] = List()) extends HasChildren[TestTree]

class MigTreeTest extends TestNGSuite {
  @Test
  def testGenerateCommands1() {
    val rootTree = TestTree(-1, List(TestTree(1)))
    val expectedCommands = List(Add[TestTree](TestTree(1), List(SpanY, Wrap)))

    val commands = MigTree.generateCommands(rootTree)
    assertEquals(commands, expectedCommands)
  }

  @Test
  def testGenerateCommands2() {
    val rootTree = TestTree(-1, List(TestTree(1),TestTree(2),TestTree(3)))
    val expectedCommands = List(
      Add[TestTree](TestTree(1), List(SpanY)),
      Add[TestTree](TestTree(2), List(SpanY)),
      Add[TestTree](TestTree(3), List(SpanY, Wrap))
      )

    val commands = MigTree.generateCommands(rootTree)
    println(commands)
    assertEquals(commands, expectedCommands)
  }

  @Test
  def testGenerateCommands3() {
    val tt = TestTree(1, List(TestTree(3)))
    val rootTree = TestTree(-1, List(tt,TestTree(2)))
    val expectedCommands = List(
      Add[TestTree](tt, List(SpanX(1))),
      Add[TestTree](TestTree(2), List(SpanY, Wrap)),
      Add[TestTree](TestTree(3), List(SpanY, Wrap))
      )

    val commands = MigTree.generateCommands(rootTree)
    println(commands)
    assertEquals(commands, expectedCommands)
  }

  @Test
  def testGenerateCommands4() {
    val tt = TestTree(1, List(TestTree(3)))
    val rootTree = TestTree(-1, List(TestTree(2), tt))
    val expectedCommands = List(
      Add[TestTree](TestTree(2), List(SpanY)),
      Add[TestTree](tt, List(SpanX(1), Wrap)),
      Add[TestTree](TestTree(3), List(SpanY, Wrap))
      )

    val commands = MigTree.generateCommands(rootTree)
    println(commands)
    assertEquals(commands, expectedCommands)
  }

  @Test
  def testGenerateCommands5() {
    val tt = TestTree(1, List(TestTree(2), TestTree(3)))
    val rootTree = TestTree(-1, List(tt))
    val expectedCommands = List(
      Add[TestTree](tt, List(SpanX(2), Wrap)),
      Add[TestTree](TestTree(2), List(SpanY)),
      Add[TestTree](TestTree(3), List(SpanY, Wrap))
      )

    val commands = MigTree.generateCommands(rootTree)
    println(commands)
    assertEquals(commands, expectedCommands)
  }

  @Test
  def testGenerateCommands6() {
    val tt3 = TestTree(3, List(TestTree(4)))
    val tt = TestTree(1, List(TestTree(2), tt3))
    val rootTree = TestTree(-1, List(tt))
    val expectedCommands = List(
      Add[TestTree](tt, List(SpanX(2), Wrap)),
      Add[TestTree](TestTree(2), List(SpanY)),
      Add[TestTree](tt3, List(SpanX(1), Wrap)),
      Add[TestTree](TestTree(4), List(SpanY, Wrap))
      )

    val commands = MigTree.generateCommands(rootTree)
    println(commands)
    assertEquals(commands, expectedCommands)
  }
}