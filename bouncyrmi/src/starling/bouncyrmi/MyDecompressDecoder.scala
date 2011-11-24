package starling.bouncyrmi

import org.jboss.netty.handler.codec.oneone.OneToOneDecoder
import org.jboss.netty.handler.codec.frame.LengthFieldBasedFrameDecoder
import org.jboss.netty.channel.{Channel, ChannelHandlerContext}
import java.util.zip.Inflater
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBufferInputStream, ChannelBuffer}

class MyDecompressDecoder(maxSize: Int, logger:(String)=>Unit) extends LengthFieldBasedFrameDecoder(maxSize, 0, 4, 0, 4) {
  override def decode(ctx: ChannelHandlerContext, channel: Channel, buffer: ChannelBuffer) = {
    val frame: ChannelBuffer = super.decode(ctx, channel, buffer).asInstanceOf[ChannelBuffer]
    if (frame == null) {
      null
    } else {
      val is = new ChannelBufferInputStream(frame)
      val avail = is.available
      val inflater = new Inflater()
      val input = new Array[Byte](avail)
      val originalByteArrayLength = is.readInt
      is.read(input)
      inflater.setInput(input)

      val uncompressedArray = new Array[Byte](originalByteArrayLength)
      inflater.inflate(uncompressedArray)
      inflater.end

      ChannelMessageSize.set(channel, avail, originalByteArrayLength)

      logger("Received compressed length: " + originalByteArrayLength)

      ChannelBuffers.wrappedBuffer(uncompressedArray)
    }
  }
}