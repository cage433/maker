package starling.collection

import starling.pivot.controller.{TreePivotFilter, TreePivotFilterNode}
import starling.pivot.{SomeSelection, AllSelection, Selection}

case class TreeNode[T](name: String, parent: Option[TreeNode[T]], value: Option[T]) {
  override def equals(obj: Any) = obj match {
    case other: TreeNode[_] => other.name == name && other.parent == parent
    case _ => false
  }

  override def hashCode = name.hashCode ^ parent.hashCode
}

case class TreeSelection[T](tree: TreeStructure[T], selection: List[TreeNode[T]]) {
  val test = selection.length > 1

  def asSelection: (TreePivotFilter, Selection) = {
    val sel = if (selection.length == 1 && tree.root == selection.head) {
      AllSelection
    } else {
      new SomeSelection(selection.toSet)
    }
    (tree.asTreePivotFilter, sel)
  }

  def selectedValues: List[T] = {
    var res = List[T]()
    selection.map{
      node => {
        tree.traverse(node) {
          n => n.value match {
            case Some(v) => res ::= v
            case _ =>
          }
        }
      }
    }
    res
  }
}

case class TreeStructure[T](treeNodes: List[TreeNode[T]]) {
  assert(treeNodes.head.parent.isEmpty, "First node in the tree should be the root: " + treeNodes)

  def root = treeNodes.head

  def traverse(node: TreeNode[T])(f: TreeNode[T] => Unit) {
    def rec(node: TreeNode[T]) {
      f(node)
      val grouped = treeNodes.groupBy(_.parent)
      val children = grouped.get(Some(node)) match {
        case None => Nil
        case Some(Nil) => Nil
        case Some(children) => {
          children.map(rec(_))
        }
      }
    }
    rec(node)
  }

  def asTreePivotFilter = {
    def rec(node: TreeNode[T]): TreePivotFilterNode = {
      val grouped = treeNodes.groupBy(_.parent)
      val children = grouped.get(Some(node)) match {
        case None => Nil
        case Some(Nil) => Nil
        case Some(children) => children.map(rec(_))
      }
      new TreePivotFilterNode(node, node.name, children)
    }
    new TreePivotFilter(rec(root))
  }
}

class TreeBuilder[T] {
  var treeNodes: List[TreeNode[T]] = List()

  def add(node: TreeNode[T]) = treeNodes ::= node

  def build: TreeStructure[T] = {
    new TreeStructure(treeNodes.reverse)
  }
}
