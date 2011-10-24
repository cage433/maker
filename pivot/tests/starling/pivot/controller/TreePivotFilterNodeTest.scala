package starling.pivot.controller

import org.testng.annotations.Test
import org.testng.Assert._


class TreePivotFilterNodeTest {
  @Test
  def mergeForestsValuesTest() {
    val initialPaths = List(
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
    val expected = List(
      TreePivotFilterNode("one", List(
        TreePivotFilterNode("one/two", List(
          TreePivotFilterNode("one/two/three", List()),
          TreePivotFilterNode("one/two/four", List())
        ))
      )),
      TreePivotFilterNode("two", List(
        TreePivotFilterNode("two/three", List())
      ))
    )
    assertEquals(TreePivotFilterNode.mergeForestsValues(initialPaths), expected)
  }
}