package maker.utils

import java.security.MessageDigest
import java.io.{FileInputStream, BufferedInputStream, File}

class Hash(val bytes:Array[Byte], val comment:String) {
  import java.util.Arrays
  def hex = Hash.bytes2Hex(bytes)
  def hashWithComment = hex + "-" + comment
  override def toString = "H:" + hashWithComment
  override def hashCode = Arrays.hashCode(bytes)
  override def equals(other:Any) = {
    other match {
      case hash:Hash => Arrays.equals(bytes, hash.bytes)
      case _ => false
    }
  }
}
object Hash {
  def fromHex(text:String) = {
    assert(text.length > 33, text + " is too short, expect hashWithComment(32)-comment")
    assert(text(32) == '-', text + " has wrong format. expect hashWithComment(32)-comment")
    val hex = text.substring(0, 32)
    val comment = text.substring(33)
    new Hash(hex2Bytes(hex), comment)
  }
  def hex2Bytes( hex: String ): Array[Byte] = {
    val pairs = (for { i <- 0 to hex.length-1 by 2}
      yield hex.substring( i, i+2 ))
    pairs.map( Integer.parseInt( _, 16 ).toByte ).toArray
  }

  def bytes2Hex( bytes: Array[Byte] ): String = {
    def cvtByte( b: Byte ): String = {
      (if (( b & 0xff ) < 0x10 ) "0" else "" ) + java.lang.Long.toString( b & 0xff, 16 )
    }
    bytes.map( cvtByte( _ )).mkString.toLowerCase
  }
  def calculateHash(file:File) = {
    val md = MessageDigest.getInstance("MD5")
    val buffer = new Array[Byte](1024)
    val input = new BufferedInputStream(new FileInputStream(file))
    Stream.continually(input.read(buffer))
      .takeWhile(_ != -1)
      .foreach(md.update(buffer, 0, _))
    input.close()
    new Hash(md.digest, file.getName)
  }
}
