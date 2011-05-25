package starling.schemaevolution

import starling.db.DBWriter
import starling.richdb.RichDB
import system.Patch
import starling.services.StarlingInit

class Patch87_UpdateSubgroupNames extends Patch {
  def patchDescription = "Updates sub group names with complete path"

  protected def runPatch(starlingInit: StarlingInit, starling:RichDB, writer:DBWriter) {
    val traders = Map(
      "Jon.Fox" -> "London Derivative Options",
      "andreas.mattsson" -> "London Derivative Options",
      "jaakko.ahmala" -> "London Derivatives",
      "seetal.patel" -> "Gasoline Spec Global",
      "matthew.tunney" -> "Houston Derivatives",
      "josh.holmes" -> "Houston Derivatives").map{case (key,value) => (key.toLowerCase -> value)}
    val sql = "select * from IntraDayTrades"
    writer.queryForUpdate(sql) {
      rs => {
        val tradeID = rs.getString("tradeID").trim
        val subgroupName = rs.getString("subgroupName").trim
        val username = rs.getString("username").trim
        val lowerCaseUsername = username.toLowerCase

        val live = (lowerCaseUsername == subgroupName.toLowerCase)
        val desk = traders.getOrElse(lowerCaseUsername, "No Desk")

        val prefix = "Oil Derivatives/" + (if (live) {
          "Live/" + desk
        } else {
          val user = username.split("\\.").map(_.capitalize).mkString(" ")
          "Scratch/" + desk + "/" + user
        })

        val newSubgroupName = prefix + "/" + subgroupName
        val newTradeID = prefix + "/" + tradeID

        rs.update(Map("tradeID" -> newTradeID, "subgroupName" -> newSubgroupName))
      }
    }
  }
}
