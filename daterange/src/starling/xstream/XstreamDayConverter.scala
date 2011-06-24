package starling.xstream

import com.thoughtworks.xstream.io.{HierarchicalStreamReader, HierarchicalStreamWriter}
import com.thoughtworks.xstream.converters.{UnmarshallingContext, MarshallingContext, Converter}
import starling.daterange.Day

class XstreamDayConverter extends Converter {
  def canConvert(theType:Class[_]) = {
    theType == classOf[Day]
  }
  def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
    writer.setValue(obj.toString)
  }
  def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
    val text = reader.getValue
    Day.quickParse(text)
  }
}