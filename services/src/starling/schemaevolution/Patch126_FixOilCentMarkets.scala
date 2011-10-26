package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.marketdata.{PriceFixingsHistoryData, PriceDataDTO, MarketDataKey}
import starling.quantity.UOM
import starling.quantity.UOM._
import starling.pivot.MarketValue
import starling.utils.sql.PersistAsBlob._
import starling.utils.sql.PersistAsBlob
import starling.instrument.utils.StarlingXStream
import collection.immutable.TreeMap
import starling.market.{Level, Market}
import starling.daterange.StoredFixingPeriod
import starling.utils.conversions.Tuple2Ordering
import starling.utils.ImplicitConversions._
import starling.gui.api.{EAIDeskInfo, Desk}

class Patch126_FixOilCentMarkets extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    val markets = List("Nymex RBOB", "NYMEX Heat")

    starling.inTransaction {
      t => {
        markets.map {
          market => {
            MigrateMarketToCents.migrate(starling, market)
          }
        }
      }
    }
  }

  override def requiresRestart = true

  object MigrateMarketToCents {
    def migrate(starling: RichDB, name: String) {
      starling.inTransaction {
        writer => {
          val updated = writer.update("update Markets set limMultiplier = 'Some(1.0)', ccy = 'USC' where Name = :name and limMultiplier = :lm and ccy = :ccy",
            Map("name" -> name, "lm" -> "Some(0.01)", "ccy" -> USD))

          assert(updated == 1, "Failed for " + name)

          val q = """
            select * from MarketData
            where
            (marketDataType = '<starling.marketdata.PriceFixingsHistoryDataType_-/>'
            or
            marketDataType = '<starling.marketdata.PriceDataType_-/>')
            and
            marketDataKey like '%>""" + name + """<%'
            """

          writer.queryForUpdate(q) {
            rs => {
              val key = rs.getObjectOption[Object]("data")
              val newKeyOption = key match {
                case Some(p : PriceDataDTO) => {
                  val prices = p.prices.map({
                    case (dr, price) => (dr -> (BigDecimal(price) * 100).toDouble)
                  })
                  Some(PriceDataDTO(prices))
                }
                case Some(p : PriceFixingsHistoryData) => {
                  var crap = new TreeMap[(Level, StoredFixingPeriod), MarketValue]()(p.fixings.ordering)
                  p.fixings.map {
                    case f@(a, MarketValue(Left(q))) => {
                      assert(q.numeratorUOM == USD)
                      crap += (a -> MarketValue(Left(q inUOM (US_CENT / q.denominatorUOM))))
                      f
                    }
                  }
                  Some(PriceFixingsHistoryData(crap))
                }
                case None => None
              }
              newKeyOption.foreach{newKey =>  rs.update(Map("data" -> PersistAsBlob(newKey)))}
            }
          }
        }
      }
    }
  }

}
