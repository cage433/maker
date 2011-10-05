package starling.eai

import starling.db.DB
import starling.dbx.QueryBuilder._
import concurrent.stm._
import starling.utils.Log

class EAIDealBookMapping(db: DB) {
  class Mapping {
    private lazy val dealToBook = db.queryWithResult(select("*") from ("tblBookDealsMap")) {
      rs => (rs.getInt("DealID") -> rs.getInt("BookID"))
    }.toMap

    def book(deal: Int): Option[Int] = dealToBook.get(deal)
  }

  private val mapping = Ref(new Mapping)

  def book(deal: Int): Int = atomic {
    implicit txn => {
      mapping().book(deal) match {
        case Some(book) => book
        case None => {
          Log.info("Deal id " + deal + " not found, reloading deals mapping.")
          mapping() = new Mapping
          mapping().book(deal) match {
            case Some(book) => book
            case None => throw new Exception("Not a recognised deal id: " + deal)
          }
        }
      }
    }
  }
}