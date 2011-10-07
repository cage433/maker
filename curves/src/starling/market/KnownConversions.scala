package starling.market

import starling.quantity.{UOM, Quantity}

trait KnownConversions {
  def convert(value: Quantity, uom: UOM): Option[Quantity]

  def convertUOM(volume : Quantity, uom : UOM) : Quantity = {
    convert(volume, uom) match {
      case Some(beqv) => beqv
      case None => throw new Exception(this + ": Couldn't convert from " + volume + " to " + uom)
    }
  }
}