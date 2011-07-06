package starling.utils

import com.rabbitmq.tools.json.{JSONWriter, JSONSerializable}

object JsonSerializable {
  class Writer(w: JSONWriter) extends JSONWriter {
    def this() = this(new JSONWriter)
    
    override def write(a: Any) = {
      // TODO [01 Apr 2010] check whether we needed to convert to Java collections for more than readability
//      if (a.isInstanceOf[Collection[_]]) {
//        w.write(a.asInstanceOf[Collection[_]])
//      } else {
        w.write(a)
//      }
    }
  }
}

trait JsonSerializable extends JSONSerializable {
  import JsonSerializable._
  override def jsonSerialize(w: JSONWriter) {
    serializeToJson(new Writer(w))
  }

  def serializeToJson(w: Writer)
}
