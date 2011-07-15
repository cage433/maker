package starling.utils

import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test
import sjson.json._

class JSONTest extends StarlingTest with ShouldMatchers {
  @Test
  def canSendAndReceiveMessagesOverRabbitMQ {
    val headers = Map("userName" -> "<userName>", "subGroupName" -> "<subGroupName>")

    val data: Array[Array[Any]] = Array(
      Array("ID", "trade date", "size", "unit", "market", "instr",
            "strike", "p/c", "period", "ex", "price", "clearing house", "counterparty",
            "broker", "strategy", "trader", "tradedFor", "comment", "entry date"),
      Array(42.0, 40553.0, 5000.0, "mt", "HSFO 180 CST Singapore vs 3.5% FuelFOB Rotterdam Barges", "Commodity Swap",
            null, null, "mar11", null, 27.0, null, "ConocoPhillips Limited",
            "Oil Brokerage Limited", 57308.0, "seetal patel", null, null, null)
    )

    val messagePayload = headers + ("data" -> data)

    toJSON(messagePayload) should equal(
      """{"userName":"<userName>","subGroupName":"<subGroupName>","data":[["ID","trade date","size","unit","market","instr","strike","p\/c","period","ex","price","clearing house","counterparty","broker","strategy","trader","tradedFor","comment","entry date"],[42.0,40553.0,5000.0,"mt","HSFO 180 CST Singapore vs 3.5% FuelFOB Rotterdam Barges","Commodity Swap",null,null,"mar11",null,27.0,null,"ConocoPhillips Limited","Oil Brokerage Limited",57308.0,"seetal patel",null,null,null]]}"""
    )
  }




  private def toJSON(value : AnyRef) = {
    new String(Serializer.SJSON.out(value))
  }
}
