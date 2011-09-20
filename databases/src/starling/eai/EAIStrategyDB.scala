package starling.eai

import starling.db.DB
import starling.dbx.QueryBuilder._
import starling.utils.cache.CacheFactory
import starling.utils.Log

class EAIStrategyDB(db: DB) {
  class Content {
    private lazy val strategies: List[TreeNode] = {
      val q = (select("id, name, parentid, isnull(sortindex, id) as sortindex") from ("tblStrategies") orderBy ("id" asc))
      db.queryWithResult(q) {
        rs => {
          val strategyID = TreeID(rs.getInt("id"))
          val name = rs.getString("name")
          val parentID = name match {
            case "Strategies" => None // "Strategies" is its own parent in the DB.. wrong
            case _ => Some(TreeID(rs.getInt("parentid")))
          }
          val sortIndex = rs.getInt("sortindex")

          TreeNode(name, strategyID, parentID, sortIndex)
        }
      }
    }
    private lazy val tree = new Tree(strategies)
    def contains(id : TreeID) = tree.contains(id)
    def pathFor(id: TreeID) = tree.pathFor(id)
    def getPath(names : List[String]): List[TreeNode] = tree.getPath(names)

    private val DealIDRegex = """.*\[(\d+)\].*""".r

    private val strategyFromDealIdCache = CacheFactory.getCache("StrategyFromDealId", unique = true)

    def getStrategyFromDealId(dealID: TreeID): Option[TreeID] = {
      strategyFromDealIdCache.memoize(
      dealID, {
        var treeID: Option[TreeID] = None

        tree.traverse() {
          case (node, _) => {
            val name = tree.nodeForID(node.entry).name
            name match {
              case DealIDRegex(matchedDealID) if matchedDealID.toInt == dealID.id => {
                assert(treeID.isEmpty, "Strategy ID matched multiple strategies: " + dealID)
                treeID = Some(node.entry)
              }
              case _ =>
            }
          }
        }
        assert(treeID.isDefined, "No strategy for deal id: " + dealID)

        treeID
      })
    }
  }

  protected val mutex = new Object
  protected var content = new Content

  def pathFor(id: TreeID) = mutex.synchronized {
    content.pathFor(id)
  }

  def getPath(names : List[String]) = content.getPath(names)
  def getIds(names : List[String]) = getPath(names).map(_.id)

  def getStrategyFromDealId(dealID: TreeID): Option[TreeID] = mutex.synchronized {
    content.getStrategyFromDealId(dealID) match {
      case s@Some(_) => s
      case None => {
        refresh
        content.getStrategyFromDealId(dealID)
      }
    }
  }

  def refresh: Unit = mutex.synchronized {
    Log.info("Refreshing EAI Strategies")
    content = new Content
  }
}