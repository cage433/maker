package starling.services.excel

import java.lang.String
import org.boris.xlloop.reflect.XLFunction
import starling.tradestore.intraday.IntradayTradeStore
import starling.auth.User
import starling.eai._
import starling.services.trade.{ExcelTradesRange, ExcelTradeReader}
import collection.immutable.List
import starling.utils.ImplicitConversions._
import starling.utils.Broadcaster
import starling.gui.api.BlotterTradeUpdate
import starling.loopyxl.ExcelMethod
import collection.mutable.ArraySeq

class TradeHandler(broadcaster : Broadcaster,
                   tradeReader : ExcelTradeReader,
                   intradayTradeStore: IntradayTradeStore,
                   traders : Traders) {

  private def generateSubgroupName(user:User, subgroup0:String) = {
    val subgroup = subgroup0.trim.toLowerCase
    val tradersToBookAndDesk = traders.bookMap
    val traderUsers = tradersToBookAndDesk.keySet

    val s = "Oil Derivatives/"
    if (traderUsers.contains(user) && ("live" == subgroup)) {
      s + "Live/" + tradersToBookAndDesk(user)._2.name + "/" + user.username
    } else {
      s + "Scratch/" + (if (traderUsers.contains(user)) {
        tradersToBookAndDesk(user)._2.name + "/" + user.name + "/" + subgroup0.trim
      } else {
        "No Desk/" + user.name + "/" + subgroup0.trim
      })
    }
  }

  @ExcelMethod
  @XLFunction(
    name = "UPLOADTRADES",
    category = "Starling",
    args = Array("range"),
    argHelp = Array("A range containing all the headers and values for the trades to upload."))
  def uploadTrades(subgroupName: String, tradesParams: Array[Array[Object]]): String = {
    assert(subgroupName.length <= 50, "The group name should be 50 characters or less")
    val user = User.currentlyLoggedOn
    val nameToUse = generateSubgroupName(user, subgroupName)
    val trades = ExcelTradesRange(subgroupName, tradesParams.head.map(h => if (h == null) "" else h.toString),
      tradesParams.tail)

    "OK:" + intradayTradeStore.storeTrades(user, subgroupName, trades.listTrades)
  }

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

    blotterTrades(user, subgroupName, header, trades)
  }

  private def blotterTrades(loggedOnUser: User, subgroupName: String, header: Array[String], trades: List[ArraySeq[Object]]): String = {
    val name = generateSubgroupName(loggedOnUser, subgroupName)

    val tradesHash: Long = {
      val allTrades = tradeReader.allTrades(header, trades, name)
      intradayTradeStore.storeTrades(loggedOnUser, name, allTrades)._1
    }

    broadcaster.broadcast(BlotterTradeUpdate(loggedOnUser, name, concat(header, trades)))

    "OK:" + tradesHash
  }

  def concat(header: Array[String], trades: List[ArraySeq[Object]]): List[List[String]] = {
    header.toList :: trades.map(_.toList.map(_.toString))
  }

  private def safeToList[T](array: Array[T]): List[T] = array match {
    case null => Nil
    case a => a.toList
  }
}

