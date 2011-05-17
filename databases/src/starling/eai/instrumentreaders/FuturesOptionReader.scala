package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.models.{American, European}
import starling.instrument.FuturesOption
import starling.quantity.{UOM, Quantity}
import starling.utils.Log

class FuturesOptionReader extends InstrumentReader {
  import EAISystemOfRecord._
  
  override def canHandle(rs: RichResultSetRow) = {
    val tradeType = rs.getInt("TradeType")
    if(rs.getInt("TradeType") == ET_OPTION || rs.getInt("TradeType") == OTC_OPTION) {
      rs.getExerciseTypeOption("optiontype") match {
        case Some(European) => true
        case Some(American) => true
        case _ => false
      }
    } else {
      false
    }
  }

  override def create(rs: RichResultSetRow) = {
    val market = rs.getFuturesMarketFromEAIQuoteID("eaiquoteid")
    val amount = rs.getDouble("Quantity")
    val (delivery, expiryDay, volume) = rs.getInt("TradeType") match {
      case ET_OPTION => {
        val delivery = rs.getDay("ContractDate").containingMonth
        val expiryDay = market.optionExpiry(delivery)
        val lotSize = market.lotSize.get
        val volume = Quantity(amount * lotSize, market.uom)
        (delivery, expiryDay, volume)
      }
      case OTC_OPTION => {
        val expiryDay = rs.getDay("ExpiryDate")
        val delivery = market.frontPeriod(expiryDay)
        val volume = Quantity(amount, market.uom)
        (delivery, expiryDay, volume)
      }
    }

    val strike = rs.getDouble("StrikePrice")
    val callPut = rs.getCallPut("CallPut")
    val optionType = rs.getExerciseType("optiontype")
    FuturesOption(market, expiryDay, delivery, Quantity(strike, market.currency / market.uom),
      volume, callPut, optionType)
  }
}
