package starling.gui.pages

import starling.pivot.controller.{TreePivotFilterNode, TreePivotFilter}
import starling.gui.StarlingLocalCache._
import starling.browser.PageContext

object IntradayGroupBuilder {
  def subgroupsToPaths(subgroups:List[String]):List[TreePivotFilterNode] = {
    subgroups.map(p => {
      val path = p.split("/").toList
      val valueAndLabels = path.zipWithIndex.map{case(s,i) => {path.take(i + 1)}}.reverse
      val endPath = valueAndLabels.head
      val endChild = TreePivotFilterNode(endPath.mkString("/"), Nil)
      valueAndLabels.tail.foldLeft(endChild)((node,value) => {
        TreePivotFilterNode(value.mkString("/"), List(node))
      })
    })
  }
}

class IntradayGroupBuilder(pageContext:PageContext) {
  val subgroups = pageContext.localCache.intradaySubgroups

  val paths = TreePivotFilterNode.mergeForestsValues(IntradayGroupBuilder.subgroupsToPaths(subgroups.keySet.toList))
  val root = paths match {
    case Nil => new TreePivotFilter(TreePivotFilterNode("None", Nil)) // Don't think this is possible.
    case first::Nil => new TreePivotFilter(first)
    case all => new TreePivotFilter(TreePivotFilterNode("All", all))
  }
}