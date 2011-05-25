package starling.bouncyrmi

import org.jboss.netty.handler.codec.oneone.OneToOneEncoder
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import java.util.zip.Deflater
import org.jboss.netty.buffer.{ChannelBufferOutputStream, ChannelBuffers, ChannelBuffer}

/**
 * level comes from Deflater statics
 */
class MyCompressEncoder(level: Int) extends OneToOneEncoder {
  val LENGTH_PLACEHOLDER = new Array[Byte](4)

  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef) = {
    msg match {
      case uncompressed: ChannelBuffer => {
        val deflator = new Deflater(level)
        val in: Array[Byte] = new Array[Byte](uncompressed.readableBytes)
        uncompressed.readBytes(in)
        deflator.setInput(in)
        deflator.finish
        val out: Array[Byte] = new Array[Byte](math.ceil(in.length * 1.001).asInstanceOf[Int] + 12)
        val outSize = deflator.deflate(out)

        val result = new ChannelBufferOutputStream(ChannelBuffers.buffer(uncompressed.order, outSize + LENGTH_PLACEHOLDER.size * 2))
        result.write(LENGTH_PLACEHOLDER)
        result.write(LENGTH_PLACEHOLDER)
        result.write(out, 0, outSize)

        result.flush
        result.close
        val encoded = result.buffer
        encoded.setInt(0, outSize + 4) // size of payload
        encoded.setInt(LENGTH_PLACEHOLDER.size, in.size) // original object size
        encoded
      }
      case _ => throw new IllegalStateException("Can't use msg: " + msg)
    }
  }
}