package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.auth.User
import starling.eai._
import starling.services.trade.ExcelTradeReader
import collection.immutable.List
import starling.utils.ImplicitConversions._
import starling.utils.Broadcaster
import starling.gui.api.BlotterTradeUpdate
import starling.loopyxl.ExcelMethod
import collection.mutable.ArraySeq
import starling.tradestore.intraday.{IntradayTradeAttributes, IntradayTradeStore}

class TradeHandler(broadcaster : Broadcaster,
                   tradeReader : ExcelTradeReader,
                   intradayTradeStore: IntradayTradeStore) {

  @ExcelMethod
  @XLFunction(
    name = "BLOTTERTRADES",
    category = "Starling",
    args = Array("range"),
    argHelp = Array("A range containing all the headers and values for the trades to upload."))
  def blotterTrades(subgroupName: String, headerAndTrades: Array[Array[Object]],
                    optionalAdditionalTrades: Array[Array[Object]]): String = {
    assert(subgroupName.length <= 50, "The group name should be 50 characters or less")

    val user = User.currentlyLoggedOn
    assert(!user.username.equalsIgnoreCase(subgroupName.trim), "Can't use your username as the group name, use 'live' instead.")

    val header = headerAndTrades.head.map(_.toString)
    val trades = (headerAndTrades.tail.toList ::: safeToList(optionalAdditionalTrades)) nullElementTo ""

    val allTrades = tradeReader.allTrades(header, trades, subgroupName)

    val tradesHash = {
      val grouped = allTrades.groupBy(t => t.attributes match {
        case i: IntradayTradeAttributes => i.subgroupName
      })

      grouped map {
        case (subgroupName, trades) => intradayTradeStore.storeTrades(user, subgroupName, trades).hash.toInt
      }
    }

    "OK:" + (0 /: tradesHash)(_^_)
  }

  def concat(header: Array[String], trades: List[ArraySeq[Object]]): List[List[String]] = {
    header.toList :: trades.map(_.toList.map(_.toString))
  }

  private def safeToList[T](array: Array[T]): List[T] = array match {
    case null => Nil
    case a => a.toList
  }
}

