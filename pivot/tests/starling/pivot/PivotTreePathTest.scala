package starling.pivot

import controller.TreePivotFilterNode
import org.scalatest.matchers.ShouldMatchers
import org.testng.TestNG


class PivotTreePathTest extends TestNG with ShouldMatchers {
  def shouldSomething {
    val abc = PivotTreePath("a/b/c")
    val xyz = PivotTreePath("d/e/f")

    val abcTree = abc.toTree
    val xyzTree = xyz.toTree

    abcTree should be ===
      TreePivotFilterNode("a",
        List(TreePivotFilterNode("b",
          List(TreePivotFilterNode("c", Nil)))))
  }
}