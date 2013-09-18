package maker.utils

import redis.clients.jedis.{Jedis,JedisPool,JedisPoolConfig}
import java.io._
import org.apache.commons.io.IOUtils
import org.apache.commons.io.output.ByteArrayOutputStream
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

class RedisPersistentCache(val hostname:String) extends PersistentCache {
  val pool = new JedisPool(new JedisPoolConfig(), hostname, 6379, 10000)
  private def withJedis[R](f: (Jedis) => R):R = {
    val jedis = pool.getResource
    try {
      f(jedis)
    } finally {
      pool.returnResource(jedis)
    }
  }
  def contains(hash : Hash) = {
    withJedis { jedis =>
      Option(jedis.get(hash.bytes)).isDefined
    } 
  }
  def get(hash: Hash):Option[List[String]] = {
    withJedis { jedis =>
      Option(jedis.get(hash.bytes)).map { bytes => {
        io.Source.fromInputStream(new GZIPInputStream(new ByteArrayInputStream(bytes))).getLines().toList
      } }
    }
  }
  def read(requests:Seq[CacheRequest]) {
    withJedis { jedis =>
      val pipeline = jedis.multi()
      val actions = requests.map( r => {
        (r,pipeline.get(r.hash.bytes))
      } )
      pipeline.exec()
      actions.foreach { case (request,response) => {
        val bytes = response.get
        if (bytes == null) throw new Exception("Not found " + request.hash)
        if (!request.file.getParentFile.exists()) request.file.getParentFile.mkdirs()
        val fos = new BufferedOutputStream(new FileOutputStream(request.file))
        try {
          IOUtils.copy(new GZIPInputStream(new ByteArrayInputStream(bytes)), fos)
        } finally {
          fos.close()
        }
      } }
    }
  }
  def put(requests:Seq[CacheRequest]) {
    withJedis { jedis =>
      val missing = {
        val pipeline = jedis.multi()
        val responses = requests.map { request => {
          (request, pipeline.exists(request.hash.bytes))
        } }
        pipeline.exec()
        responses.flatMap { case (request, response) => {
          if (response.get) None else Some(request)
        } }
      }
      val pipeline = jedis.pipelined()
      missing.foreach { request => {
        val fis = new FileInputStream(request.file)
        val baos = new ByteArrayOutputStream()
        val gzip = new GZIPOutputStream(baos)
        IOUtils.copy(fis, gzip)
        fis.close()
        gzip.close()
        pipeline.set(request.hash.bytes, baos.toByteArray)
      } }
      pipeline.sync()
    }
  }
  def put(hash:Hash, inputStream:InputStream) {
    withJedis { jedis =>
      if (!jedis.exists(hash.bytes)) {
        val baos = new ByteArrayOutputStream()
        val gzip = new GZIPOutputStream(baos)
        IOUtils.copy(inputStream, gzip)
        gzip.close()
        jedis.set(hash.bytes, baos.toByteArray)
      }
    }
  }
}
object RedisPersistentCache {
  private var _instance:Option[RedisPersistentCache] = None
  def instance(hostname:String) = {
    synchronized {
      if (_instance == None) {
        _instance = Some(new RedisPersistentCache(hostname))
      } else {
        assert(_instance.get.hostname == hostname)
      }
      _instance.get
    }
  }
  def main(args:Array[String]) {
    val redis = instance("localhost")
    val requests = new File("maker/target/classes/maker").listFiles().toList.flatMap {
       file => if (file.isFile) Some(CacheRequest(Hash.calculateHash(file), file)) else None
    }
    redis.put(requests)
    println("DONE")
  }
}
