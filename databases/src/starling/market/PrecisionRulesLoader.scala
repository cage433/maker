package starling.market

import rules.{Precision, PrecisionRules}
import starling.db.DB
import starling.dbx.QueryBuilder._


class PrecisionRulesLoader(eai: DB) extends PrecisionRules {
  lazy val rules = {
    val q = select("id, defaultPrecision, clearportdefaultprecision") from ("eai.dbo.tblquotes")
    eai.queryWithResult(q) {
      rs => {
        val quoteId = rs.getInt("id")
        val default = rs.getInt("defaultPrecision")
        val clearport = rs.getInt("clearportdefaultprecision")
        (quoteId -> Precision(default, clearport))
      }
    } toMap
  }

  def rule(eaiQuoteID: Int) = {
    rules.get(eaiQuoteID)
  }
}