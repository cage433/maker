package starling.pivot.controller

import org.testng.annotations.Test
import org.testng.Assert._


class TreePivotFilterNodeTest {
  @Test
  def mergeForestsValuesTest() {
    val initialPaths = List(
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
    val expected = List(
      TreePivotFilterNode("one", "one", List(
        TreePivotFilterNode("one/two", "two", List(
          TreePivotFilterNode("one/two/three", "three", List()),
          TreePivotFilterNode("one/two/four", "four", List())
        ))
      )),
      TreePivotFilterNode("two", "two", List(
        TreePivotFilterNode("two/three", "three", List())
      ))
    )
    assertEquals(TreePivotFilterNode.mergeForestsValues(initialPaths), expected)
  }
}