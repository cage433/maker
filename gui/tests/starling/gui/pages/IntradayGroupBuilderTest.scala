package starling.gui.pages

import org.testng.annotations.Test
import org.testng.Assert._
import starling.pivot.controller.TreePivotFilterNode

class IntradayGroupBuilderTest {
  @Test
  def subgroupsToPathsTest() {
    val paths = List("one/two/three", "one/two/four", "two/three")
    val expected = List(
      TreePivotFilterNode("one", List(
        TreePivotFilterNode("one/two", List(
          TreePivotFilterNode("one/two/three", List())
        ))
      )),
      TreePivotFilterNode("one", List(
        TreePivotFilterNode("one/two", List(
          TreePivotFilterNode("one/two/four", List())
        ))
      )),
      TreePivotFilterNode("two", List(
        TreePivotFilterNode("two/three", List())
      ))
    )

    assertEquals(IntradayGroupBuilder.subgroupsToPaths(paths), expected)
  }
}