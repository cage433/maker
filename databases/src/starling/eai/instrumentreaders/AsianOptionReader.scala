package starling.eai.instrumentreaders

import starling.systemofrecord.InstrumentReader
import starling.richdb.RichResultSetRow
import starling.instrument.AsianOption
import starling.quantity.{UOM, Quantity}

class AsianOptionReader extends InstrumentReader {
  import EAISystemOfRecord._

  override def canHandle(rs: RichResultSetRow) = {
    if (rs.getInt("TradeType") == ET_OPTION || rs.getInt("TradeType") == OTC_OPTION) {
      rs.getString("optiontype") == ASIAN_TYPE ||
              rs.getString("optiontype") == AVERAGE_TYPE
    } else {
      false
    }
  }

  override def create(rs: RichResultSetRow) = {
    val index = rs.getSingleIndexFromEAIQuoteID("eaiquoteid")
    val delivery = rs.hasColumn("ContractDate") match {
      case true => rs.getDay("ContractDate").containingMonth // TODO [28 Apr 2010] is this correct?
      case false => rs.getDateRange("AveragingStartDate", "AveragingEndDate")
    }
    val ccy = index.currency.inBaseCurrency
    val uom = index.uom
    // Convert cents/USD
    val strike = Quantity(rs.getDouble("StrikePrice"), ccy / uom) inUOM index.priceUOM
    val amount = rs.getInt("TradeType") match {
      case ET_OPTION => {
        val lotSize = index.lotSize.get
        rs.getDouble("Quantity") * lotSize
      }
      case OTC_OPTION => rs.getDouble("Quantity")
    }

    val callPut = rs.getCallPut("CallPut")

    AsianOption(index, index.makeAveragingPeriodMonthIfPossible(delivery), strike, Quantity(amount, index.uom), callPut)
  }
}
