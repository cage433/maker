package starling.schemaevolution

import starling.market.{Market, ForwardMarket, FuturesMarket}
import starling.instrument._
import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.utils.ImplicitConversions._
import starling.utils.sql.QueryBuilder._
import starling.daterange.{Month, Day}
import starling.utils.Log
import starling.services.StarlingInit

class Patch14_RefactorTradeTablesForFuturesChange_STR58 extends Patch {
  def addColumn(writer: DBWriter, table: String) = {
    writer.update("""
        ALTER TABLE %s
        ADD [delivery] [char](20) NULL
            """ % (table))
    writer.update("""
        ALTER TABLE %s
        ADD [tradeNotUnderstood] [tinyint] default 0 not null
            """ % (table))
  }

  def dropColumns(writer: DBWriter, table: String) = {
//    writer.update("""
//        ALTER TABLE %s
//        drop column [lastTradingDay]
//            """ % (table))
    writer.update("""
        ALTER TABLE %s
        drop column [deliveryDay]
            """ % (table))
  }

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {

    // redundant since we removed old Trinity code
    throw new Exception("Patch31_FixLMEOptionTenors should not be run")

//    Log.info("This patch will take about 15 minutes")
//
//    writer.update("drop table Indexes")
//    writer.update("truncate table EAITrade")
//
//    writer.update("""
//        ALTER TABLE EAITrade
//        ADD [delivery] [char](20) NULL
//            """)
//
//    val tables = List("TrinityTrade", "GalenaTrade")
//    for (table <- tables) {
//      addColumn(writer, table)
//
//      val q = (
//              select("*")
//                      from (table)
//                      where (("lastTradingDay" isNotNull) or ("deliveryDay" isNotNull))
//              )
//
//      var updated = 0
//      Log.info("Updating " + table)
//      var warnings = Set[String]()
//      starling.query(q) {
//        rs => {
//          val id = rs.getInt("id")
//          updated += 1
//          rs.getString("instrument") match {
//
//            // Futures
//            case Future.name => {
//              assert(rs.isNull("deliveryDay"))
//              // Some futures should now be forwards
//              rs.getMarket("market") match {
//                // future on a forward market - rewrite as a forward
//                case f: ForwardMarket => {
//                  val lastTradingDay = rs.getDay("lastTradingDay")
//                  writer.update(table, Map("instrument" -> CommodityForward.name, "delivery" -> lastTradingDay), ("id" eql id))
//                }
//
//                case f: FuturesMarket => {
//                  val lastTradingDay = f.tenor match {
//                    // if it's a daily market the lastTradingDay is actually the delivery day
//                    case Day => f.lastTradingDay(rs.getDay("lastTradingDay"))
//                    case Month => rs.getDay("lastTradingDay")
//                  }
//                  if (!f.validLastTradingDay(lastTradingDay)) {
//                    // if we are a future on a futures market but haven't the correct last trading day
//                    // then we're actually a forward on a futures market - mark as not understood
//                    val delivery = rs.getDay("lastTradingDay")
//                    writer.update(table, Map("instrument" -> CommodityForward.name, "delivery" -> delivery, "tradeNotUnderstood" -> 1), ("id" eql id))
//                    val warning = ("Future", f, delivery).toString
//                    if (!warnings.contains(warning)) {
//                      println(warning)
//                      warnings += warning
//                    }
//                  } else {
//                    // just a normal future that makes sense
//                    val period = f.frontPeriod(lastTradingDay)
//                    assert(period.tenor.get == Day || period.tenor.get == Month)
//                    writer.update(table, Map("delivery" -> period), ("id" eql id))
//                  }
//                }
//              }
//            }
//
//            // Futures Options
//            case FuturesOption.name => {
//              assert(rs.isNull("deliveryDay"))
//              val expiryDay = rs.getDay("exerciseDay")
//
//              rs.getMarket("market") match {
//                // if we're a futures option on a forward market rewrite as a forward option
//                case f: ForwardMarket => {
//                  val delivery = rs.getDay("lastTradingDay")
//                  writer.update(table, Map("instrument" -> ForwardOption.name, "delivery" -> delivery), ("id" eql id))
//                }
//                case f: FuturesMarket => {
//                  val lastTradingDay = f.tenor match {
//                  // if it's a daily market the lastTradingDay is actually the delivery day
//                    case Day => f.lastTradingDay(rs.getDay("lastTradingDay"))
//                    case Month => rs.getDay("lastTradingDay")
//                  }
//
//                  if (!f.hasOptions || !f.validLastTradingDay(lastTradingDay) || !f.validOptionExpiry(expiryDay, lastTradingDay)) {
//                    // the market doesn't have options or we don't have the correct last trading day or expiry day for that market
//                    // rewrite as forward option and mark as not understood
//                    val delivery = rs.getDay("lastTradingDay")
//                    writer.update(table, Map("instrument" -> ForwardOption.name, "delivery" -> delivery, "tradeNotUnderstood" -> 1), ("id" eql id))
//                    val warning = ("Futures Option", f, lastTradingDay, expiryDay).toString
//                    if (!warnings.contains(warning)) {
//                      println(warning)
//                      warnings += warning
//                    }
//                  } else {
//                    // a futures option on a futures market
//                    // at the moment the lastTradingDay is the last trading day of the delivery period. Or the delivery period
//                    // for daily markets
//                    val maturity = lastTradingDay
//                    val period = f.frontPeriod(maturity)
//                    assert(expiryDay == f.optionExpiry(period), expiryDay + "!=" + f.optionExpiry(period))
//                    assert(period.tenor.get == Day || period.tenor.get == Month)
//                    writer.update(table, Map("delivery" -> period), ("id" eql id))
//                  }
//                }
//              }
//            }
//
//            case CommodityForward.name => {
//              assert(rs.isNull("lastTradingDay"))
//              val deliveryDay = rs.getDay("deliveryDay")
//              val market = rs.getMarket("market") match {
//                case Market.COMEX_GOLD => Market.LBMA_GOLD
//                case Market.COMEX_SILVER => Market.LBMA_SILVER
//                case m => m
//              }
//              writer.update(table, Map("delivery" -> deliveryDay, "Market" -> market), ("id" eql id))
//            }
//
//            case UnpricedAverage.name => {
//              assert(rs.isNull("lastTradingDay"))
//              val deliveryDay = rs.getDay("deliveryDay")
//              writer.update(table, Map("delivery" -> deliveryDay), ("id" eql id))
//            }
//          }
//        }
//        if (updated % 10000 == 0) {
//          Log.info("Updated " + updated)
//        }
//      }
//      dropColumns(writer, table)
//    }
  }

  def patchDescription = "Change the trades tables to have a delivery period instead of a lastTradingDay"
}