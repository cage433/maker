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
      TreePivotFilterNode(null, "a",
        List(TreePivotFilterNode(null, "b",
          List(TreePivotFilterNode(null, "c", Nil)))))
  }
}