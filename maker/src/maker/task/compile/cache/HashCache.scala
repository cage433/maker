package maker.utils

import java.security.MessageDigest
import java.io.{FileInputStream, BufferedInputStream, File}
import java.util.Comparator

object HashCache {

  private val cache = new java.util.concurrent.ConcurrentHashMap[File,(Long,Hash)]()

  def hash(file:File):Hash = {
    val result = cache.get(file)
    val lastModified = file.lastModified()
    if (result != null && result._1 == lastModified) {
      result._2
    } else {
      val h = Hash.calculateHash(file)
      cache.put(file, (lastModified, h))
      h
    }
  }

  def hash(files:List[File]):Hash = {
    val md = MessageDigest.getInstance("MD5")
    val array = files.toArray
    java.util.Arrays.sort(array, new Comparator[File]() { def compare(a: File, b: File) = a.compareTo(b) })
    array.foreach { file => {
      if (file.getName.endsWith(".jar")) {
        md.update(file.getName.getBytes("UTF8"))
      } else {
        val hash = HashCache.hash(file)
        md.update(hash.bytes)
      }
    } }
    new Hash(md.digest, "listing")
  }
}
