package starling.schemaevolution

import collection.mutable.HashMap

import com.thoughtworks.xstream.io.{HierarchicalStreamReader, HierarchicalStreamWriter}
import com.thoughtworks.xstream.converters.{UnmarshallingContext, MarshallingContext, Converter}

import starling.instrument.utils.StarlingXStream


/** Class for patching case classes, only works if the case class attributes are primative */
class CaseClassPatcher[CC](cc: Map[String, String] => CC)(implicit m: Manifest[CC]) {
  def patch(xml: String): String = ordinaryStream.toXML(patchingStream.fromXML(xml))

  private val ordinaryStream = StarlingXStream.createXStream
  private val patchingStream = StarlingXStream.createXStream

  patchingStream.registerConverter(new Converter {
    def canConvert(theType: Class[_]) = theType == m.erasure

    def marshal(obj: Object, writer: HierarchicalStreamWriter, context: MarshallingContext) {
      writer.setValue(ordinaryStream.toXML(obj))
    }

    def unmarshal(reader: HierarchicalStreamReader , context: UnmarshallingContext) = {
      val map = new HashMap[String, String]

      while (reader.hasMoreChildren) {
        reader.moveDown();
        map.put(reader.getNodeName, reader.getValue)
        reader.moveUp();
      }

      cc(map.toMap).asInstanceOf[Object]
    }
  })
}