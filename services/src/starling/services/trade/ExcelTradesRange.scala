package starling.services
package trade

import starling.db.IntradayTradeSystem
import starling.instrument._
import starling.tradestore.intraday.IntradayTradeAttributes
import starling.daterange._
import starling.richdb.RichInstrumentResultSetRow
import starling.eai.{TreeID, EAIStrategyDB}
import starling.models.{Put, Call, CallOrPut}
import starling.market.{Market, Index}
import starling.quantity.{SpreadQuantity, UOM, Quantity, Percentage}
import starling.trade.{TradeID, Trade}

case class ExcelTradesRange(subgroupName : String,
                            headers: Array[String],
                            values: Array[Array[Object]]) {
  // the list of headers (in normalised form) which we (might?) need to make the instrument
  val instrumentHeaders = ("Instrument" :: InstrumentType.fields).map(ExcelTradeAttributes.normalForm)

  // all other headers will become part of the trade attributes. in original form.
  val availableHeaders = headers.filterNot(s =>
          instrumentHeaders.contains(ExcelTradeAttributes.normalForm(s.stripSuffix("UOM")))
    ).toList

  case class DataRow(i : Int) {
    def getPercentage(name: String) = Percentage(getDouble(name))
    def getString(name: String) = getObject(name).toString
    def getIndexFromName(name: String) = Index.fromName(getString(name))
    def getMarket(name: String) = Market.fromName(getString(name))
    def getCallPut(name: String) : CallOrPut = {
      getString(name).toLowerCase match {
        case "c" | "call" => Call
        case "p" | "put" => Put
        case other => throw new Exception("Unrecognised value for call/put " + other + " expect call/put/c/p")
      }
    }
    def getDay(name: String):Day = {
      getObject(name) match {
        case d:java.lang.Double => Day.fromExcel(d.doubleValue)
        case s:String => Day.parse(s)
        case other => throw new Exception("Can't turn " + other + " into a day")
      }
    }

    def getDateRange(name : String, tenor:Option[TenorType]=None):DateRange = {
      getObject(name) match {
        case d:java.lang.Double => {
          val day = Day.fromExcel(d.doubleValue)
          tenor match {
            case None => day
            case Some(t) => t.containing(day)
          }
        }
        case s:String => DateRange.parse(s)
      }
    }

    def getQuantity(name: String) = {
      val value = getDouble(name)
      val uom = getUOM(name + "UOM")
      Quantity(value, uom)
    }

    def getStrikes: SpreadQuantity = {
      SpreadQuantity.parse(getString("strike") + " " + getString("strikeUOM")) match {
        case Some(s) => s
        case None => throw new Exception("Couldn't parse: " + getString("strike"))
      }
    }

    def getDouble(name: String) : Double = {
      getObject(name) match {
        case d: java.lang.Double => d.doubleValue
        case s: String => s.toDouble
        case other => throw new Exception("The value " + other + " for " + name + " is not a double")
      }
    }
    def getUOM(name: String) : UOM = {
      UOM.fromString(getString(name))
    }
    def exists(name: String) = {
      val maybeIndex = maybeIndexOf(name)
      maybeIndex != -1 && values(i)(maybeIndex) != null
    }

    private def maybeIndexOf(name:String) = {
      val normalisedName = normalisedForm(name)
      headers.findIndexOf(normalisedForm(_) == normalisedName)
    }
    def getObject(name:String) : Object = {
      val maybeIndex = maybeIndexOf(name)
      if (maybeIndex >= 0) {
        val row = values(i)
        val cellValue = row(maybeIndex)
        if (cellValue == null) {
          throw new Exception("No value is specified for the column " + name)
        } else {
          cellValue
        }
      } else {
        throw new Exception("There is no column with the heading " + name)
      }
    }
  }

  def normalisedForm(s : String) : String = s.filterNot(_ == ' ').toLowerCase

  def countTrades = {
    // at a first approximation it's the number of rows with something in the instrument column
    val instrumentHeader = normalisedForm("Instrument")
    val maybeIndex = headers.findIndexOf(normalisedForm(_) == instrumentHeader)
    if (maybeIndex >= 0) {
      values.map((x:Array[Object]) => if (x(maybeIndex) == null) 0 else 1).reduceLeft(_+_)
    } else {
      0
    }
  }

  def listTrades : List[Trade] = {
    val counterparty = "MSFT"
    val sys = IntradayTradeSystem
    val defaultBook : TreeID = TreeID(0)

    (0 until values.length).flatMap((i: Int) => {
      val tradeID = TradeID(subgroupName + i.formatted("%03d"), sys)
      val row = DataRow(i)
      val rowLike = new RichInstrumentResultSetRow {
        def getFuturesSpreadMarket(name: String) = unimplemented
        def getCMEEuroDollarMarket = unimplemented
        def getPaymentFrequency(name: String) = unimplemented
        def getDayCount(name: String) = unimplemented
        def getPercentage(name: String) = row.getPercentage(name)
        def getString(name: String) = row.getString(name)
        def getIndexFromName(name: String) = row.getIndexFromName(name)
        def getMarket(name: String) = row.getMarket(name)
        def getCallPut(name: String) = row.getCallPut(name)
        def getDay(name: String) = row.getDay(name)
        def getUOM(name: String) = row.getUOM(name)
        def getQuantity(name: String) = row.getQuantity(name)
        def getStrikes = row.getStrikes
        def getForwardMarket(name: String) = Market.forwardMarketFromName(row.getString(name))
        def getFuturesMarket(name: String) = Market.futuresMarketFromName(row.getString(name))
        def getDateRange(name: String, tenor : Option[TenorType] = None) = row.getDateRange(name, tenor)
        override def getSpread(name:String) = {
          row.getObject(name) match {
            case d:java.lang.Double => {
              val day = Day.fromExcel(d.doubleValue)
              val month = Month(day.year, day.month)
              Spread(month, month.next)
            }
            case _ => super.getSpread(name)
          }
        }
        def getMonth(name: String) = row.getDateRange(name).firstMonth
        def getTimestamp(name: String) = unimplemented
        def getBoolean(name: String) = row.getString(name).toBoolean
        def getInt(name: String) = row.getString(name).toInt
        def getDouble(name: String) = row.getDouble(name)
        def isNull(name: String) = unimplemented
        def getObject[T](column: String) = unimplemented

        override def getExerciseType(name: String) = super.getExerciseType(name)
        override def getDeliveryDay(name: String) = super.getDeliveryDay(name)

        def unimplemented = throw new IllegalStateException("ExcelLoopReceiver: unimplemented.")
      }
      val tradeDay = if (row.exists("Trade Day")) row.getDay("Trade Day") else Day.today
      val strategyID = None
      val attributes = new IntradayTradeAttributes(strategyID, defaultBook, None, "", "", "", "", "", subgroupName, tradeDay, "")
      val instrument = try {
        if (row.exists("Instrument")) {
          val instrumentName = row.getString("Instrument")
          val instrumentType = TradeableType.fromName(instrumentName)
          //HACK to ignore blank rows in Special Sits spreadsheet
          if (instrumentType == NetEquityPosition && row.getString("RIC") == "0.0") {
            None
          } else {
            Some(instrumentType.createTradeable(rowLike))
          }
        } else {
          None
        }
      } catch {
        case e : Exception => println("Error reading trade from excel"); e.printStackTrace; Some(new ErrorInstrument(e.toString))
      }

      instrument.map(new Trade(tradeID, tradeDay, counterparty, attributes, _))
    }).toList
  }
}

// TODO: move to appropriate location
