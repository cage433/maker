package starling.eai

import starling.db.DB
import starling.dbx.QueryBuilder._

class EAIDealBookMapping(db: DB) {
  private lazy val dealToBook = db.queryWithResult(select("*") from ("tblBookDealsMap")) {
    rs => (rs.getInt("DealID") -> rs.getInt("BookID"))
  }.toMap

  def book(deal: Int): Int = dealToBook.get(deal) match {
    case Some(book) => book
    case None => throw new Exception("Not a recognised deal id: " + deal)
  }
}