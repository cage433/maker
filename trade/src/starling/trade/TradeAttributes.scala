package starling.trade

import starling.richdb.RichInstrumentResultSetRow
import starling.pivot.{FieldDetails, Field}

import starling.utils.ImplicitConversions._


trait TradeAttributes {
//  def keyList: List[String] = keys.toList.sortWith(_<_)
//
  def createFieldValues : Map[Field,Any] = {
    details.map { case(name,value) => Field(name) -> value }
  }
  def details : Map [String,Any]

//  def getOrBlank(key: String) = get(key) match {
//    case Some(v) => v
//    case None => UndefinedValue
//  }

//  def asMap = self
//
//  def details = Map() ++ keyList.map(key => { key -> getOrBlank(key) })
}
