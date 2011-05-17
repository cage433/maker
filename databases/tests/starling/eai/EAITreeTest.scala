package starling.eai

import starling.utils.StarlingTest
import org.testng.annotations.Test
import org.testng.Assert._
import org.scalatest.matchers.ShouldMatchers

class EAITreeTest extends StarlingTest with ShouldMatchers {
  val root = TreeID(0)
  val b11 = TreeID(11)
  val b12 = TreeID(12)
  val b21 = TreeID(21)
  val b22 = TreeID(22)
  val b212 = TreeID(212)
  val b311 = TreeID(311)
  val rootNode = TreeNode("root", root, None)
  val b11Node = TreeNode("b11", b11, Some(root))
  val b12Node = TreeNode("b12", b12, Some(root))
  val b21Node = TreeNode("b21", b21, Some(b11))
  val b22Node = TreeNode("b22", b22, Some(b12))
  val b212Node = TreeNode("b212", b212, Some(b11))
  val b311Node = TreeNode("b311", b311, Some(b21))

  lazy val tree = new Tree(List(
    rootNode,
    b11Node,
    b12Node,
    b21Node,
    b22Node,
    b212Node,
    b311Node
  ))

  @Test
  def testTree {
    // simple sanity checks
    assertEquals(root, tree.tree.entry)
    assertEquals(None, tree.tree.parent)
    assertEquals(2, tree.tree.children.get.size)
    assertTrue(tree.tree.children.get.exists(_.entry == b11))
    assertTrue(tree.tree.children.get.exists(_.entry == b12))

    // check the children of root
    val rootChildren = tree.children(root).get
    assertEquals(2, rootChildren.size)
    assertTrue(rootChildren.exists(_ == b11))
    assertTrue(rootChildren.exists(_ == b12))

    // check the children of b21
    val b21Children = tree.children(b21).get
    assertEquals(1, b21Children.size)
    assertTrue(b21Children.exists(_ == b311))

    // check the children of b11
    val b11Children = tree.children(b11).get
    assertEquals(2, b11Children.size)
    assertTrue(b11Children.exists(_ == b212))
    assertTrue(b11Children.exists(_ == b21))

    assertEquals(tree.children(b311), None)
  }

  @Test
  def testEAIBook = {
    assertEquals(tree.nodeForID(b11), b11Node)
    assertEquals(tree.nodeForID(b311), b311Node)
    assertTrue(try {
      tree.nodeForID(TreeID(1234))
      false
    }catch {
      case e => true
    })
  }

  @Test
  def testReverse = {
    val parents = tree.parents(tree.entryToNodeMap(b311)) { node => node.entry }

    assertEquals(parents, List(root, b11, b21, b311))
  }

  @Test
  def testNodes {
    val children = tree.children(tree.entryToNodeMap(b11)) { node => node.entry }

    assertEquals(children, List(b311, b21, b212, b11))
  }

  @Test
  def testPathsOfNodeShouldIncludeParentNodes {
    tree.getPath(List("root", "b11", "b21", "b311")) should be === List(rootNode, b11Node, b21Node, b311Node)
    tree.getPath(List("root", "b11", "b21"))         should be === List(rootNode, b11Node, b21Node)
    tree.getPath(List("root", "b11"))                should be === List(rootNode, b11Node)
    tree.getPath(List("root"))                       should be === List(rootNode)
  }
}
