package starling.gui.pages

import starling.gui.PageContext
import starling.pivot.controller.{TreePivotFilterNode, TreePivotFilter}

object IntradayGroupBuilder {
  def subgroupsToPaths(subgroups:List[String]):List[TreePivotFilterNode] = {
    subgroups.map(p => {
      val path = p.split("/").toList
      val valueAndLabels = path.zipWithIndex.map{case(s,i) => {
        (path.take(i + 1).mkString("/"), path(i))
      }}.reverse
      val (endPath, endLabel) = valueAndLabels.head
      val endChild = TreePivotFilterNode(endPath, endLabel, List())
      valueAndLabels.tail.foldLeft(endChild)((node,valueAndLabel) => {
        TreePivotFilterNode(valueAndLabel._1, valueAndLabel._2, List(node))
      })
    })
  }
}

class IntradayGroupBuilder(pageContext:PageContext) {
  val subgroups = pageContext.localCache.intradaySubgroups

  val paths = TreePivotFilterNode.mergeForestsValues(IntradayGroupBuilder.subgroupsToPaths(subgroups.keySet.toList))
  val root = paths match {
    case Nil => new TreePivotFilter(TreePivotFilterNode("None", "None", List())) // Don't think this is possible.
    case first::Nil => new TreePivotFilter(first)
    case all => new TreePivotFilter(TreePivotFilterNode("All", "All", all))
  }
}