package starling.eai

import starling.db.DB
import starling.dbx.QueryBuilder._

class EAIBookDB(db: DB) {
  lazy val books: List[(TreeID, String, Option[TreeID])] = {
    val q = (select ("id, name, parentid") from ("tblBooks") orderBy ("id" asc))
    db.queryWithResult(q) {
      rs => {
        val bookID = TreeID(rs.getInt("id"))
        val name = rs.getString("name")
        val parentID = name match {
          case "Books" => None // "Books" is its own parent in the DB.. wrong
          case _ => Some(TreeID(rs.getInt("parentid")))
        }
        (bookID, name, parentID)
      }
    }
  }

}