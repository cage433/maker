package starling.gui.pages

import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot.controller.TreePivotFilterNode

class IntradayGroupBuilderTest {
  @Test
  def subgroupsToPathsTest() {
    val paths = List("one/two/three", "one/two/four", "two/three")
    val expected = List(
      TreePivotFilterNode("one", "one", List(
        TreePivotFilterNode("one/two", "two", List(
          TreePivotFilterNode("one/two/three", "three", List())
        ))
      )),
      TreePivotFilterNode("one", "one", List(
        TreePivotFilterNode("one/two", "two", List(
          TreePivotFilterNode("one/two/four", "four", List())
        ))
      )),
      TreePivotFilterNode("two", "two", List(
        TreePivotFilterNode("two/three", "three", List())
      ))
    )

    assertEquals(IntradayGroupBuilder.subgroupsToPaths(paths), expected)
  }
}