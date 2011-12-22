package starling.instrument

import starling.richdb.RichInstrumentResultSetRow
import starling.pivot.{FieldDetails, Field}

import starling.utils.ImplicitConversions._


trait TradeAttributes {
  def createFieldValues : Map[Field,Any] = {
    persistedDetails.map { case(name,value) => Field(name) -> value }
  }

  def persistedDetails : Map [String,Any]

}
