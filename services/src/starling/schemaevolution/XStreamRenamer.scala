package starling.schemaevolution

import system.Patch
import starling.db.DBWriter
import starling.richdb.RichDB
import starling.services.StarlingInit
import starling.instrument.utils.StarlingXStream
import starling.marketdata.{PriceDataDTO, PriceData}
import starling.utils.ImplicitConversions._


class XStreamRenamer(renamed: (String, Class[_])*) {
  def rename(xml: String): String = ordinaryStream.toXML(renamingStream.fromXML(xml))

  private val renamingStream = StarlingXStream.createXStream.update { xstream =>
    renamed.map { case (from, to) => xstream.alias(from, to) }
  }
  private val ordinaryStream = StarlingXStream.createXStream
}


