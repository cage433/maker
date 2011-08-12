package starling.xstream

import com.thoughtworks.xstream.io.{HierarchicalStreamReader, HierarchicalStreamWriter}
import com.thoughtworks.xstream.converters.{UnmarshallingContext, MarshallingContext, Converter}
import starling.quantity.UOM

class XstreamUOMConverter extends Converter {
  def canConvert(theType:Class[_]) = {
    classOf[UOM] == theType
  }
  def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
    writer.setValue(obj.asInstanceOf[UOM].identifier)
  }
  def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
    if (reader.getValue == "") UOM.NULL else UOM.fromIdentifier(reader.getValue)
  }
}