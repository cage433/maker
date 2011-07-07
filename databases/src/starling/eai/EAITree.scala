package starling.eai

import starling.pivot.{PivotTreePath}
import collection.immutable.List
import starling.utils.AssertionUtil
import starling.utils.ImplicitConversions._

case class TreeNode(name:String, id:TreeID, parentId:Option[TreeID], sortIndex : Int = 0) {
  def isRoot = parentId.isEmpty
}

/**
 * entries: (This ID, This name, ID or Parent)
 */
class Tree(entries: List[TreeNode]) {
  lazy protected val nodeMap: Map[TreeID, TreeNode] = Map() ++ entries.map{case treeNode => (treeNode.id -> treeNode)}

  val childrenMap:scala.collection.Map[Option[TreeID],scala.collection.immutable.List[TreeNode]] = entries.groupBy(_.parentId)

  lazy val entryToNodeMap: Map[TreeID, Node] = Map() ++ children(tree) { node => (node.entry -> node) }
  lazy val treeIDToPathMap: Map[TreeID, PivotTreePath] = Map() ++ entryToNodeMap.map { case (id, _) => {
    id -> PivotTreePath(parents(entryToNodeMap(id)) { node => nodeMap(node.entry).name })
  } }

  def contains(id: TreeID) = entryToNodeMap.contains(id)
  def pathFor(id: TreeID) = treeIDToPathMap(id)
  def nodeForID(id:TreeID) = nodeMap(id)

  /**
   * Get ID for a given book name, or None if it can't be found.
   */
  def parseBook(name : String) : Option[TreeNode] = {
    // this isn't the most efficient way of doing it - but at the moment this doesn't get
    // called very often.
    nodeMap.find(tuple => tuple._2.name == name).map(_._2)
  }


  /**
   *  Prints the tree. Looks like:
   * EAIBook(Books,BookID(0))
   * --EAIBook(Houston Derivatives,BookID(190))
   * --EAIBook(DT,BookID(179))
   * ----EAIBook(Shipping,BookID(182))
   * ----EAIBook(Bunkering,BookID(181))
   * etc..
   */
  def print {
    traverse(tree) {
      (node, depth) => println(("--" * depth) + nodeMap(node.entry))
    }
  }

  /**
   * A node in the tree. Optional parent and children.
   */
  case class Node(parent: Option[TreeID], children: Option[List[Node]], entry: TreeID) {
    def +(child: Node) = {
      children match {
        case Some(l) => Node(parent, Some(child :: l), entry)
        case None => Node(parent, Some(List(child)), entry)
      }
    }

    private def allPaths(path : List[TreeID])(node : Node) : List[List[TreeID]] = {
      val prefixedPath = node.entry :: path

      node.children match {
        case None => List(prefixedPath)
        case Some(Nil) => List(prefixedPath)
        case Some(subTrees) => subTrees.flatMap(subTree => allPaths(prefixedPath)(subTree)) ::: List(prefixedPath)
      }
    }

    lazy val paths: Map[List[String], List[TreeNode]] =
      allPaths(Nil)(this).map(_.reverse.map(nodeMap(_))).toMapWithKeys(_.map(_.name))

    import AssertionUtil._
    def getPath(names : List[String]) = names match {
      case Nil => Nil
      case _ => paths.getOrElse(names, fail("Should be able to find path for names: " + names))
    }
  }

  /**
   * The root node of the tree.
   */
  lazy val tree: Node = {
    def recurse(parent: Option[TreeID], entry: TreeID, entries: List[TreeNode]): Node = {
      childrenMap.get(Some(entry)) match {
        case None => Node(parent, None, entry)
        case Some(children) => (Node(parent, None, entry) /: children) {
          (node, child) => { node + recurse(Some(entry), child.id, entries) }
        }
      }
    }
    recurse(None, root.id, entries)
  }

  lazy val root = entries.find(_.isRoot).get

  /**
   * The children for a given book, if any. Doesn't return grandchildren etc.
   */
  def children(entry: TreeID): Option[List[TreeID]] = {
    // find the node in the tree for entry
    def find(node: Node): Option[Node] = {
      if(node.entry == entry)
        Some(node)
      else
        node.children match {
          case None => None
          case Some(children) => {
            children.flatMap(find) match {
              case Nil => None
              case v :: Nil => Some(v)
              case _ => throw new Exception("Multiple matches")
            }
          }
        }
    }
    find(tree) match {
      case None => throw new Exception("No node with key " + entry + " found.")
      case Some(node) => node.children match {
        case None => None
        case Some(list) => Some(list.map(_.entry))
      }
    }
  }

  def children[A](start : Node)(f : Node => A = identity _) : List[A] = {
    var nodes = List[A]()

    traverse(start) { (node, _) => { nodes ::= f(node) } }

    nodes
  }

  def parents[A](start: Node = tree)(f : Node => A = identity _) : List[A] = {
    var nodes = List[A]()

    reverseTraverse(start) { node => { nodes ::= f(node) } }

    nodes
  }

  /**
   * Traverses the tree calling f at each node with the params of node and depth
   */
  def traverse(start: Node = tree)(f:(Node, Int)=>Unit) {
    def recurse(node: Node, depth: Int) {
      f(node, depth)
      node.children match {
        case None =>
        case Some(children) => for (child <- children) {
          recurse(child, depth + 1)
        }
      }
    }
    recurse(start, 0)
  }

  def reverseTraverse(start: Node = tree)(f:(Node)=>Unit) {
    def recurse(node: Node) {
      f(node)
      node.parent match {
        case None =>
        case Some(parent) => recurse(entryToNodeMap(parent))
      }
    }
    recurse(start)
  }

  def getPath(names : List[String]) = tree.getPath(names)
}

