package starling.richdb

import starling.curves.interestrate.{DayCount}
import starling.market._
import rules.{NoPricingRule, SwapPricingRule}
import starling.models._
import starling.daterange._
import starling.quantity._

trait RichInstrumentResultSetRow {
  def getInt(name:String):Int
  def getDouble(name:String):Double
  def getBoolean(name:String):Boolean
  def getTimestamp(name:String):Timestamp
  def getQuantity(name : String) : Quantity
  def getUOM(name : String) : UOM
  def getDay(name : String) : Day
  def getDateRange(name : String, tenor : Option[TenorType] = None) : DateRange
  def getSpread(name: String) = {
    val periods = getString(name).split("/")
    new Spread( Month.parse(periods(0).trim), Month.parse(periods(1).trim) )
  }
  def getPeriod(name : String): Period = {
    getString(name) match {
      case Period(p) => p
    }
  }
  def getStrikes: SpreadQuantity
  def getMonth(name : String) : Month
  def getDeliveryDay(name : String) : Day = getDateRange(name) match {
    case d: Day => d
    case o => throw new Exception(o + " is not a day")
  }
  def getCallPut(name : String) : CallOrPut
  def getExerciseType(name : String) = getExerciseTypeOption(name) match {
    case Some(et) => et
    case t => throw new Exception("Unknown option type: " + t)
  }
  def getExerciseTypeOption(name : String) = getString(name).toLowerCase match {
    case "eto" => Some(American)
    case "european" => Some(European)
    case "american" => Some(American)
    case t => None
  }
  def getSwapPricingRule(name: String): SwapPricingRule = getString(name) match {
    case SwapPricingRule(r) => r
  }
  def getSwapPricingRule(name: String, default: String): SwapPricingRule = getString(name) match {
    case null => getString(default) match {
      case SwapPricingRule(r) => r
    }
    case SwapPricingRule(r) => r
  }
  def getMarket(name : String) : Market
  def getCommodityMarket(name : String) : CommodityMarket
  def getFuturesMarket(name : String) : FuturesMarket
  def getFuturesSpreadMarket(name : String) : FuturesSpreadMarket
  def getIndexFromName(name : String) : Index
  def getSingleIndexFromName(column: String) =  Index.singleIndexFromName(getString(column))
  def getString(name : String) : String
  def getStringOrNone(name : String) : Option[String] = if (isNull(name)) None else Some(getString(name))
  def getObject[T](column: String): T
  def getObjectOrElse[T](column: String, orElse: T): T = isNull(column) match {
    case true => orElse
    case false => getObject(column)
  }
  def getPercentage(name : String) : Percentage
  def getDayCount(name : String) : DayCount
  def isNull(name : String) : Boolean
  def getStringOrBlank(name : String) = if (isNull(name)) "" else getString(name)
}
